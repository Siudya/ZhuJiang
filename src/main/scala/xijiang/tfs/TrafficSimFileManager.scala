package xijiang.tfs

import org.chipsalliance.cde.config.Parameters
import xijiang.NodeType
import xs.utils.FileRegisters
import zhujiang.ZJParametersKey
import zhujiang.chi.ChannelEncodings

case class TrafficSimParams(
  rxReadyMaxDelay: Int = 10,
  txValidMaxDelay: Int = 5
)

object TrafficSimFileManager {
  def release(p: Parameters): Unit = {
    FileRegisters.add("env/tfs/include", "traffic_sim.h", header, true)
    FileRegisters.add("env/tfs/src", "traffic_sim.cpp", source(p), true)
    FileRegisters.add("env/tfs/src", "main.cpp", mainStr, true)
    FileRegisters.add("", "xmake.lua", compScr, true)
  }

  def header: String =
    s"""
       |#ifndef __TRAFFIC_SIM_H__
       |#define __TRAFFIC_SIM_H__
       |#include "svdpi.h"
       |
       |#ifdef __cplusplus
       |extern "C" {
       |#endif
       |
       |void tfs_get_tx_flit(short int node_id, char chn, svBitVecVal *flit, svBit *valid, svBit ready, svBit reset);
       |
       |void tfs_get_rx_ready(short int node_id, char chn, svBit valid, svBit *ready, svBit reset);
       |
       |void tfs_step();
       |
       |void tfs_set_seed(unsigned int seed);
       |
       |void tfs_init();
       |
       |void tfs_verbose();
       |
       |#ifdef __cplusplus
       |}
       |#endif
       |
       |#endif""".stripMargin

  def source(p: Parameters) = {
    val params = p(ZJParametersKey)
    s"""
       |#include "traffic_sim.h"
       |#include "svdpi.h"
       |#include "traffic_board.h"
       |#include <cstdint>
       |#include <cstdio>
       |#include <cstring>
       |#include <memory>
       |#include <random>
       |#include <unordered_map>
       |#include <vector>
       |
       |#define FLIT_SIZE ${params.maxFlitBits}
       |#define TIME_OUT ${params.tfbParams.get.timeOut}
       |#define NODE_NID_BITS ${params.nodeNidBits}
       |#define NODE_TYPE_BITS ${params.nodeTypeBits}
       |#define NODE_NET_BITS ${params.nodeNetBits}
       |#define FLIT_BUF_SIZE ${(params.maxFlitBits + 7) / 8}
       |
       |#define R_TYPE ${NodeType.R}
       |#define HF_TYPE ${NodeType.HF}
       |#define HI_TYPE ${NodeType.HI}
       |#define C_TYPE ${NodeType.C}
       |
       |#define REQ ${ChannelEncodings.REQ}
       |#define RSP ${ChannelEncodings.RSP}
       |#define DAT ${ChannelEncodings.DAT}
       |#define SNP ${ChannelEncodings.SNP}
       |
       |#define RX_READY_MAX_DELAY ${params.tfsParams.get.rxReadyMaxDelay}
       |#define TX_VALID_MAX_DELAY ${params.tfsParams.get.txValidMaxDelay}
       |
       |#define NODE_TYPE_OFF NODE_NID_BITS
       |#define NODE_NET_OFF (NODE_NID_BITS + NODE_TYPE_BITS)
       |
       |#define NODE_ID_BITS (NODE_NID_BITS + NODE_TYPE_BITS + NODE_NET_BITS)
       |#define TGT_ID_OFF 4
       |#define SRC_ID_OFF (TGT_ID_OFF + NODE_ID_BITS)
       |
       |#define TFS_ERR(...)                  \\
       |  {                                   \\
       |    TrafficSim::get_instance().info_lock.lock();                                   \\
       |    fprintf(stderr, "[TFS ERROR] @ %lu: ", TrafficSim::get_instance().global_timer); \\
       |    fprintf(stderr, __VA_ARGS__);     \\
       |    fflush(stderr);                   \\
       |    TrafficSim::get_instance().info_lock.unlock();                                   \\
       |  }
       |#define TFS_INFO(...)                 \\
       |  {                                   \\
       |    TrafficSim::get_instance().info_lock.lock();                                   \\
       |    fprintf(stdout, "[TFS INFO] @ %lu: ", TrafficSim::get_instance().global_timer); \\
       |    fprintf(stdout, __VA_ARGS__);     \\
       |    fflush(stdout);                   \\
       |    TrafficSim::get_instance().info_lock.unlock();                                   \\
       |  }
       |
       |using namespace std;
       |
       |class NodeManager {
       |  public:
       |  uint16_t node_id;
       |  uint16_t pool_type = 0;
       |  mt19937 random_gen;
       |  unordered_map<uint8_t, vector<uint16_t>> legal_tgt_pool;
       |  unordered_map<uint8_t, array<uint8_t, FLIT_BUF_SIZE>> chn_tx_flit_map;
       |  unordered_map<uint8_t, uint32_t> chn_rx_ready_timer_map;
       |  unordered_map<uint8_t, uint32_t> chn_tx_valid_timer_map;
       |  NodeManager(uint16_t nid);
       |  void step();
       |  void rx_fire(uint8_t chn);
       |  void tx_fire(uint8_t chn);
       |};
       |
       |typedef vector<unordered_map<uint8_t, vector<uint16_t>>>
       |  tgt_pool_t;
       |
       |class TrafficSim {
       |  private:
       |  TrafficSim() = default;
       |  bool initialized = false;
       |
       |  public:
       |  TrafficSim(const TrafficSim &) = delete;
       |  TrafficSim &operator=(const TrafficSim &) = delete;
       |  tgt_pool_t legal_tgt_pool;
       |  unordered_map<uint16_t, unique_ptr<NodeManager>> node_mng_pool;
       |  bool verbose = false;
       |  mutex info_lock;
       |
       |  uint64_t global_timer = 0;
       |
       |  static TrafficSim &get_instance() {
       |    static TrafficSim instance;
       |    return instance;
       |  }
       |  void init();
       |  void set_seed(uint32_t seed);
       |  void step();
       |};
       |
       |inline uint64_t get_field(uint64_t vec, uint8_t offset, uint8_t width) {
       |  return (vec >> offset) & ((1 << width) - 1);
       |}
       |
       |inline uint64_t clear_field(uint64_t vec, uint8_t offset, uint8_t width) {
       |  uint64_t clear = ~(((1 << width) - 1) << offset);
       |  return vec & clear;
       |}
       |
       |NodeManager::NodeManager(uint16_t nid) {
       |  node_id = nid;
       |  auto &tfs = TrafficSim::get_instance();
       |  bool csn = get_field(node_id, NODE_NET_OFF, NODE_NET_BITS) == 1;
       |  bool c2c = get_field(node_id, NODE_TYPE_OFF, NODE_TYPE_BITS) == C_TYPE;
       |  if(c2c) {
       |    pool_type = 2;
       |  } else if(csn) {
       |    pool_type = 1;
       |  } else {
       |    pool_type = 0;
       |  }
       |
       |  for(const auto &[k, v] : tfs.legal_tgt_pool[pool_type]) {
       |    legal_tgt_pool[k] = vector<uint16_t>();
       |    for(const auto &tgt : v) {
       |      if(tgt != node_id) legal_tgt_pool[k].push_back(tgt);
       |    }
       |  }
       |  chn_tx_flit_map[REQ] = array<uint8_t, FLIT_BUF_SIZE>();
       |  chn_tx_flit_map[RSP] = array<uint8_t, FLIT_BUF_SIZE>();
       |  chn_tx_flit_map[DAT] = array<uint8_t, FLIT_BUF_SIZE>();
       |  chn_tx_flit_map[SNP] = array<uint8_t, FLIT_BUF_SIZE>();
       |  for(auto &[chn, _]: chn_tx_flit_map) tx_fire(chn);
       |  chn_rx_ready_timer_map[REQ] = 0;
       |  chn_rx_ready_timer_map[RSP] = 0;
       |  chn_rx_ready_timer_map[DAT] = 0;
       |  chn_rx_ready_timer_map[SNP] = 0;
       |  chn_tx_valid_timer_map[REQ] = 0;
       |  chn_tx_valid_timer_map[RSP] = 0;
       |  chn_tx_valid_timer_map[DAT] = 0;
       |  chn_tx_valid_timer_map[SNP] = 0;
       |
       |  
       |  printf("  node: 0x%x\\n", node_id);
       |  for(const auto &[k, v]: legal_tgt_pool) {
       |    if(k == REQ) {
       |      printf("    REQ: ");
       |    } else if(k == RSP) {
       |      printf("    RSP: ");
       |    } else if(k == DAT) {
       |      printf("    DAT: ");
       |    } else {
       |      printf("    SNP: ");
       |    }
       |    for(const auto &d: v){
       |      printf("0x%x ", d);
       |    }
       |    printf("\\n");
       |  }
       |}
       |
       |void NodeManager::rx_fire(uint8_t chn) {
       |  uniform_int_distribution<uint8_t> dist(0, RX_READY_MAX_DELAY);
       |  if(chn > 4) {
       |    TFS_ERR("illegal channel type %d!\\n", chn);
       |  } else {
       |    chn_rx_ready_timer_map[chn] = dist(random_gen);
       |  }
       |}
       |
       |void NodeManager::tx_fire(uint8_t chn) {
       |  if(chn > 4) {
       |    TFS_ERR("illegal channel type %d!\\n", chn);
       |    return;
       |  }
       |  uint8_t *flit = chn_tx_flit_map[chn].data();
       |  uniform_int_distribution<uint8_t> dist_flit(0, 0xFF);
       |  for(int i = 0; i < FLIT_BUF_SIZE - 1; i++) flit[i] = dist_flit(random_gen);
       |  flit[FLIT_BUF_SIZE - 1] = 0;
       |  uint8_t tgt_pool_size = legal_tgt_pool[chn].size();
       |  uniform_int_distribution<uint8_t> dist_tgt_pos(0, tgt_pool_size - 1);
       |  uint8_t pos = dist_tgt_pos(random_gen);
       |  uint16_t tgt_id = legal_tgt_pool[chn].at(pos);
       |  
       |  uint64_t *head_ptr = (uint64_t *)flit;
       |  head_ptr[0] = clear_field(head_ptr[0], TGT_ID_OFF, NODE_ID_BITS) | (tgt_id << TGT_ID_OFF);
       |  uniform_int_distribution<uint8_t> dist_valid(0, TX_VALID_MAX_DELAY);
       |  chn_tx_valid_timer_map[chn] = dist_valid(random_gen);
       |}
       |
       |void NodeManager::step() {
       |  for(auto &[_, t]:chn_rx_ready_timer_map) t = (t == 0) ? t : t - 1;
       |  for(auto &[_, t]:chn_tx_valid_timer_map) t = (t == 0) ? t : t - 1;
       |}
       |
       |void TrafficSim::init() {
       |  if(initialized) return;
       |  for(int i = 0; i < 3; i++) {
       |    legal_tgt_pool.push_back(unordered_map<uint8_t, vector<uint16_t>>());
       |    legal_tgt_pool[i][REQ] = vector<uint16_t>();
       |    legal_tgt_pool[i][RSP] = vector<uint16_t>();
       |    legal_tgt_pool[i][DAT] = vector<uint16_t>();
       |    legal_tgt_pool[i][SNP] = vector<uint16_t>();
       |  }
       |
       |  uint8_t lrn_id_num = tfb_get_nodes_size(0x00);
       |  uint8_t lhf_id_num = tfb_get_nodes_size(0x01);
       |  uint8_t lhi_id_num = tfb_get_nodes_size(0x02);
       |  uint8_t crn_id_num = tfb_get_nodes_size(0x10);
       |  uint8_t chf_id_num = tfb_get_nodes_size(0x11);
       |  uint8_t c2c_id_num = tfb_get_nodes_size(0x13);
       |
       |  uint16_t *lrn_id_arr = new uint16_t[lrn_id_num];
       |  uint16_t *lhf_id_arr = new uint16_t[lhf_id_num];
       |  uint16_t *lhi_id_arr = new uint16_t[lhi_id_num];
       |  uint16_t *crn_id_arr = new uint16_t[crn_id_num];
       |  uint16_t *chf_id_arr = new uint16_t[chf_id_num];
       |  uint16_t *c2c_id_arr = new uint16_t[c2c_id_num];
       |
       |  tfb_get_nodes(0x00, lrn_id_arr);
       |  tfb_get_nodes(0x01, lhf_id_arr);
       |  tfb_get_nodes(0x02, lhi_id_arr);
       |  tfb_get_nodes(0x10, crn_id_arr);
       |  tfb_get_nodes(0x11, chf_id_arr);
       |  tfb_get_nodes(0x13, c2c_id_arr);
       |
       |  for(uint8_t i = 0; i < lrn_id_num; i++) {
       |    legal_tgt_pool[0][RSP].push_back(lrn_id_arr[i]);
       |    legal_tgt_pool[0][DAT].push_back(lrn_id_arr[i]);
       |    legal_tgt_pool[0][SNP].push_back(lrn_id_arr[i]);
       |  }
       |  for(uint8_t i = 0; i < lhf_id_num; i++) {
       |    legal_tgt_pool[0][REQ].push_back(lhf_id_arr[i]);
       |    legal_tgt_pool[0][RSP].push_back(lhf_id_arr[i]);
       |    legal_tgt_pool[0][DAT].push_back(lhf_id_arr[i]);
       |  }
       |  for(uint8_t i = 0; i < lhi_id_num; i++) {
       |    legal_tgt_pool[0][REQ].push_back(lhi_id_arr[i]);
       |    legal_tgt_pool[0][RSP].push_back(lhi_id_arr[i]);
       |    legal_tgt_pool[0][DAT].push_back(lhi_id_arr[i]);
       |  }
       |  for(uint8_t i = 0; i < crn_id_num; i++) {
       |    legal_tgt_pool[2][RSP].push_back(crn_id_arr[i]);
       |    legal_tgt_pool[2][DAT].push_back(crn_id_arr[i]);
       |    legal_tgt_pool[2][SNP].push_back(crn_id_arr[i]);
       |  }
       |  for(uint8_t i = 0; i < chf_id_num; i++) {
       |    legal_tgt_pool[2][REQ].push_back(chf_id_arr[i]);
       |    legal_tgt_pool[2][RSP].push_back(chf_id_arr[i]);
       |    legal_tgt_pool[2][DAT].push_back(chf_id_arr[i]);
       |  }
       |  for(uint8_t i = 0; i < c2c_id_num; i++) {
       |    uint8_t chip_id = get_field(c2c_id_arr[i], 0, NODE_NID_BITS);
       |    uint16_t csn_remote_requester =
       |      (1 << NODE_NET_OFF) | (R_TYPE << NODE_TYPE_OFF) | chip_id;
       |    uint16_t csn_remote_home =
       |      (1 << NODE_NET_OFF) | (HF_TYPE << NODE_TYPE_OFF) | chip_id;
       |
       |    legal_tgt_pool[1][RSP].push_back(csn_remote_requester);
       |    legal_tgt_pool[1][DAT].push_back(csn_remote_requester);
       |    legal_tgt_pool[1][SNP].push_back(csn_remote_requester);
       |
       |    legal_tgt_pool[1][REQ].push_back(csn_remote_home);
       |    legal_tgt_pool[1][RSP].push_back(csn_remote_home);
       |    legal_tgt_pool[1][DAT].push_back(csn_remote_home);
       |  }
       |  printf("tfs node manager target:\\n");
       |  for(uint8_t i = 0; i < lrn_id_num; i++) node_mng_pool[lrn_id_arr[i]] = make_unique<NodeManager>(lrn_id_arr[i]);
       |  for(uint8_t i = 0; i < lhf_id_num; i++) node_mng_pool[lhf_id_arr[i]] = make_unique<NodeManager>(lhf_id_arr[i]);
       |  for(uint8_t i = 0; i < lhi_id_num; i++) node_mng_pool[lhi_id_arr[i]] = make_unique<NodeManager>(lhi_id_arr[i]);
       |  for(uint8_t i = 0; i < crn_id_num; i++) node_mng_pool[crn_id_arr[i]] = make_unique<NodeManager>(crn_id_arr[i]);
       |  for(uint8_t i = 0; i < chf_id_num; i++) node_mng_pool[chf_id_arr[i]] = make_unique<NodeManager>(chf_id_arr[i]);
       |  for(uint8_t i = 0; i < c2c_id_num; i++) node_mng_pool[c2c_id_arr[i]] = make_unique<NodeManager>(c2c_id_arr[i]);
       |
       |  delete[] lrn_id_arr;
       |  delete[] lhf_id_arr;
       |  delete[] lhi_id_arr;
       |  delete[] crn_id_arr;
       |  delete[] chf_id_arr;
       |  delete[] c2c_id_arr;
       |  initialized = true;
       |}
       |
       |void TrafficSim::set_seed(uint32_t seed) {
       |  uniform_int_distribution<uint32_t> dist(0, 9999);
       |  mt19937 random_gen(seed);
       |  for(auto &[_, mng]: node_mng_pool) mng->random_gen.seed(dist(random_gen));
       |}
       |
       |void TrafficSim::step() {
       |  global_timer++;
       |  tfb_step();
       |  for(auto &[_, v] : node_mng_pool) {
       |    v->step();
       |  }
       |}
       |
       |extern "C" {
       |void tfs_get_tx_flit(short int node_id, char chn, svBitVecVal *flit, svBit *valid, svBit ready, svBit reset) {
       |  auto &tfs = TrafficSim::get_instance();
       |  const auto mng_ptr = tfs.node_mng_pool[node_id].get();
       |  auto tx_flit_ptr = mng_ptr->chn_tx_flit_map[chn].data();
       |  if(reset == 1) {
       |    *valid = 0;
       |  } else {
       |    if(*valid == 1 && ready == 1) mng_ptr->tx_fire(chn);
       |    *valid = (mng_ptr->chn_tx_valid_timer_map[chn] == 0) ? 1 : 0;
       |  }
       |  memcpy(flit, tx_flit_ptr, FLIT_BUF_SIZE);
       |}
       |
       |void tfs_get_rx_ready(short int node_id, char chn, svBit valid, svBit *ready, svBit reset) {
       |  const auto mng_ptr = TrafficSim::get_instance().node_mng_pool[node_id].get();
       |  if(reset == 1) {
       |    *ready = 0;
       |  } else {
       |    if(valid == 1 && *ready == 1) mng_ptr->rx_fire(chn);
       |    *ready = (mng_ptr->chn_rx_ready_timer_map[chn] == 0) ? 1 : 0;
       |  }
       |}
       |
       |void tfs_step() {
       |  TrafficSim::get_instance().step();
       |}
       |
       |void tfs_set_seed(unsigned int seed) {
       |  TrafficSim::get_instance().set_seed(seed);
       |}
       |
       |void tfs_init() {
       |  TrafficSim::get_instance().init();
       |}
       |
       |void tfs_verbose() {
       |  TrafficSim::get_instance().verbose = true;
       |  tfb_verbose();
       |}
       |}""".stripMargin
  }

  def compScr: String =
    s"""
       |add_rules("mode.debug", "mode.release")
       |add_rules("plugin.compile_commands.autoupdate", {outputdir = ".vscode"})
       |set_policy("check.auto_ignore_flags", false)
       |
       |set_languages("c11", "cxx17")
       |set_toolchains("clang")
       |
       |add_requires("argparse")
       |
       |target("VeriRing")
       |  set_toolchains("@verilator")
       |  add_rules("verilator.static")
       |  add_includedirs("env/tfb/include")
       |  add_values("verilator.flags", "--top-module", "TrafficSimTop", "--trace")
       |  add_values("verilator.flags", "--no-timing", "--threads", "4", "--threads-dpi", "all")
       |  add_values("verilator.flags", "+define+ASSERT_VERBOSE_COND_=1", "+define+STOP_COND_=1")
       |  add_files("rtl/*.sv")
       |
       |target("emu")
       |  set_kind("binary")
       |  add_includedirs("env/tfb/include")
       |  add_includedirs("env/tfs/include")
       |  add_files("env/tfb/src/*.cpp")
       |  add_files("env/tfs/src/*.cpp")
       |  add_cxxflags("-march=native")
       |  add_deps("VeriRing")
       |  add_packages("argparse")""".stripMargin

  def mainStr: String =
    s"""
       |#include "argparse/argparse.hpp"
       |#include "traffic_sim.h"
       |#include "VeriRing.h"
       |#include <string>
       |#include "verilated.h"
       |#include "verilated_vcd_c.h"
       |#include <cstdio>
       |
       |using namespace std;
       |
       |class SimMain {
       |  private:
       |  SimMain();
       |  int reset_cycle = 100;
       |  bool dump_wave = false;
       |  uint64_t max_cycle = 0;
       |  uint64_t global_timer = 0;
       |  uint64_t print_interval = 0;
       |  bool verbose = false;
       |  string wave_file = "sim.vcd";
       |  uint32_t seed = 1234;
       |
       |  public:
       |  SimMain(const SimMain &) = delete;
       |  SimMain &operator=(const SimMain &) = delete;
       |  VeriRing* ring_ptr;
       |  VerilatedVcdC* waveform_dumper;
       |  argparse::ArgumentParser argparser;
       |  ~SimMain();
       |
       |  static SimMain &get_instance() {
       |    static SimMain instance;
       |    return instance;
       |  }
       |  void step();
       |  void reset();
       |  void parse(int argc, char *argv[]);
       |  bool time_out();
       |};
       |
       |SimMain::~SimMain() {
       |  ring_ptr->final();
       |  if(dump_wave) {
       |    waveform_dumper->close();
       |    delete waveform_dumper;
       |  }
       |}
       |
       |void SimMain::reset() {
       |  ring_ptr->reset = 1;
       |  int i = reset_cycle;
       |  while(i --> 0) step();
       |  ring_ptr->reset = 0;
       |}
       |
       |SimMain::SimMain() {
       |  argparser.add_description("Traffic Sim options");
       |  argparser.add_argument("-s", "--seed").help("Random seed for simulation").default_value(1043).scan<'i', int>();;
       |  argparser.add_argument("-w", "--wave").help("Waveform path").default_value(string("sim.vcd"));
       |  argparser.add_argument("-c", "--cycle").help("Simulation cycles").default_value(10000).scan<'i', int>();
       |  argparser.add_argument("-r", "--reset").help("Reset cycles").default_value(100).scan<'i', int>();
       |  argparser.add_argument("-d", "--dump-wave").help("Do dump wave").flag();
       |  argparser.add_argument("-v", "--verbose").help("Do dump wave").flag();
       |}
       |
       |void SimMain::parse(int argc, char *argv[]) {
       |  try {
       |    argparser.parse_args(argc, argv);
       |  } catch(const std::exception &err) {
       |    std::cerr << err.what() << std::endl;
       |    std::cerr << argparser;
       |    std::exit(1);
       |  }
       |  wave_file = argparser.get<string>("--wave");
       |  dump_wave = argparser.get<bool>("--dump-wave");
       |  seed = argparser.get<int>("--seed");
       |  max_cycle = argparser.get<int>("--cycle");
       |  reset_cycle = argparser.get<int>("--reset");
       |  print_interval = max_cycle / 100;
       |  verbose = argparser.get<bool>("--verbose");
       |
       |  printf("Options: Status\\n");
       |  printf("seed: %d\\n", seed);
       |  printf("wave_file: %s\\n", wave_file.c_str());
       |  printf("max_cycle: %ld\\n", max_cycle);
       |  printf("reset_cycle: %d\\n", reset_cycle);
       |  printf("dump_wave: %d\\n", dump_wave);
       |  printf("verbose: %d\\n", verbose);
       |  fflush(stdout);
       |
       |  ring_ptr = new VeriRing();
       |  if(dump_wave){
       |    Verilated::traceEverOn(true);
       |    waveform_dumper = new VerilatedVcdC();
       |    ring_ptr->trace(waveform_dumper, 0);
       |    waveform_dumper->open(wave_file.c_str());
       |  }
       |  ring_ptr->reset = 1;
       |  ring_ptr->clock = 0;
       |  ring_ptr->eval();
       |  tfs_init();
       |  tfs_set_seed(seed);
       |  if(verbose) tfs_verbose();
       |
       |  reset();
       |}
       |
       |void print_progress() {
       |  static int progress = 0;
       |  printf("\\rTrafficSim [");
       |  int i = progress;
       |  while(i --> 0) printf("=");
       |  printf(">");
       |  i = 100 - progress;
       |  while(i --> 0) printf(" ");
       |  printf("] %d%%", progress);
       |  progress++;
       |  fflush(stdout);
       |}
       |
       |void SimMain::step() {
       |  ring_ptr->clock = 1;
       |  ring_ptr->eval();
       |  tfs_step();
       |  global_timer++;
       |  if(dump_wave) waveform_dumper->dump(2 * global_timer);
       |  ring_ptr->clock = 0;
       |  ring_ptr->eval();
       |  if(dump_wave) waveform_dumper->dump(2 * global_timer + 1);
       |  if(global_timer % print_interval == 0 && !verbose) print_progress();
       |}
       |
       |bool SimMain::time_out() {
       |  return global_timer > max_cycle;
       |}
       |
       |int main(int argc, char *argv[]) {
       |  SimMain &sim_main = SimMain::get_instance();
       |  sim_main.parse(argc, argv);
       |  while(!sim_main.time_out()) sim_main.step();
       |  printf("\\nSim End\\n");
       |  return 0;
       |}
       |""".stripMargin
}
