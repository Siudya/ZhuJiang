package xijiang.tfs

import org.chipsalliance.cde.config.Parameters
import xijiang.tfb.TrafficBoardFileManager.{allNodeTypeDefs, allNodeTypeMap}
import xijiang.{Node, NodeType}
import xs.utils.FileRegisters
import zhujiang.ZJParametersKey
import zhujiang.chi.ChannelEncodings

case class TrafficSimParams(
  rxReadyMaxDelay: Int = 5,
  txValidMaxDelay: Int = 5
)

object TrafficSimFileManager {
  def release(p: Parameters): Unit = {
    FileRegisters.add("env/tfs/include", "traffic_sim.h", header, true)
    FileRegisters.add("env/tfs/src", "traffic_sim.cpp", source(p), true)
    FileRegisters.add("env/tfs/src", "main.cpp", mainStr, true)
    FileRegisters.add("", "xmake.lua", compScr, true)
  }

  private val localNodeTypes: String = allNodeTypeMap.filter(_._3 == "LOCAL_TGT_POOL").map(_._1.toUpperCase + "_TYPE").reduce((a: String, b: String) => s"$a, $b")
  private val csnNodeTypes: String = allNodeTypeMap.filter(_._3 == "C2C_TGT_POOL").map(_._1.toUpperCase + "_TYPE").reduce((a: String, b: String) => s"$a, $b")
  private val ejectsArraysInitFn: String = allNodeTypeMap.map(
    elm => s"  type_ejects_map[${elm._1.toUpperCase}_TYPE] = {${elm._4.reduce((a: String, b: String) => s"$a, $b")}};\n"
  ).reduce(_ + _)

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
       |#define NODE_AID_BITS ${params.nodeAidBits}
       |#define NODE_NET_BITS ${params.nodeNetBits}
       |#define FLIT_BUF_SIZE ${(params.maxFlitBits + 7) / 8}
       |
       |$allNodeTypeDefs
       |
       |#define REQ ${ChannelEncodings.REQ}
       |#define RSP ${ChannelEncodings.RSP}
       |#define DAT ${ChannelEncodings.DAT}
       |#define SNP ${ChannelEncodings.SNP}
       |#define ERQ ${ChannelEncodings.ERQ}
       |
       |#define TYPE_NET_OFF ${NodeType.width}
       |#define TYPE_NET_BITS 1
       |
       |#define NODE_AID_BITS ${params.nodeAidBits}
       |#define NODE_NID_BITS ${params.nodeNidBits}
       |#define NODE_ID_BITS ${params.nodeIdBits}
       |#define TGT_ID_OFF 4
       |#define SRC_ID_OFF (TGT_ID_OFF + NODE_ID_BITS)
       |
       |#define RX_READY_MAX_DELAY ${params.tfsParams.get.rxReadyMaxDelay}
       |#define TX_VALID_MAX_DELAY ${params.tfsParams.get.txValidMaxDelay}
       |
       |#define TGT_ID_OFF 4
       |#define SRC_ID_OFF (TGT_ID_OFF + NODE_ID_BITS)
       |#define TXN_ID_BITS 12
       |#define TXN_ID_OFF (4 + NODE_ID_BITS + NODE_ID_BITS)
       |
       |#define LOCAL_TGT_POOL 0
       |#define CSN_TGT_POOL 1
       |#define C2C_TGT_POOL 2
       |
       |#define TFS_ERR(...)                  \\
       |  {                                   \\
       |    TrafficSim::get_instance().info_lock.lock();                                   \\
       |    fprintf(stderr, "\\n[TFS ERROR] @ %lu: ", TrafficSim::get_instance().global_timer); \\
       |    fprintf(stderr, __VA_ARGS__);     \\
       |    fflush(stderr);                   \\
       |    TrafficSim::get_instance().info_lock.unlock();                                   \\
       |  }
       |#define TFS_INFO(...)                 \\
       |  {                                   \\
       |    TrafficSim::get_instance().info_lock.lock();                                   \\
       |    fprintf(stdout, "\\n[TFS INFO] @ %lu: ", TrafficSim::get_instance().global_timer); \\
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
       |  uint16_t txn_id = 0;
       |  bool csn = false;
       |  mt19937 random_gen;
       |  unordered_map<uint8_t, vector<uint16_t>> legal_tgt_pool;
       |  unordered_map<uint8_t, array<uint8_t, FLIT_BUF_SIZE>> chn_tx_flit_map;
       |  unordered_map<uint8_t, uint32_t> chn_rx_ready_timer_map;
       |  unordered_map<uint8_t, uint32_t> chn_tx_valid_timer_map;
       |  NodeManager(uint16_t nid, uint8_t nt);
       |  void init();
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
       |  TrafficSim();
       |  bool initialized = false;
       |  const uint8_t local_node_types[${allNodeTypeMap.count(_._3 == "LOCAL_TGT_POOL")}] = {$localNodeTypes};
       |  const uint8_t csn_node_types[${allNodeTypeMap.count(_._3 == "C2C_TGT_POOL")}] = {$csnNodeTypes};
       |  unordered_map<uint8_t, vector<uint8_t>> type_ejects_map;
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
       |TrafficSim::TrafficSim() {
       |$ejectsArraysInitFn
       |}
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
       |NodeManager::NodeManager(uint16_t nid, uint8_t nt) {
       |  node_id = nid;
       |  txn_id = 0;
       |  bool c2c = nt == C2C_TYPE;
       |  csn = get_field(nt, TYPE_NET_OFF, TYPE_NET_BITS) == 0x1;
       |  if(c2c) {
       |    pool_type = C2C_TGT_POOL;
       |  } else if(csn) {
       |    pool_type = CSN_TGT_POOL;
       |  } else {
       |    pool_type = LOCAL_TGT_POOL;
       |  }
       |}
       |
       |void NodeManager::init() {
       |  const auto &tfs = TrafficSim::get_instance();
       |  for(const auto &[k, v] : tfs.legal_tgt_pool.at(pool_type)) {
       |    legal_tgt_pool[k] = vector<uint16_t>();
       |    for(const auto &tgt : v) {
       |      if(tgt != node_id) legal_tgt_pool[k].push_back(tgt);
       |    }
       |  }
       |  chn_tx_flit_map[REQ] = array<uint8_t, FLIT_BUF_SIZE>();
       |  chn_tx_flit_map[RSP] = array<uint8_t, FLIT_BUF_SIZE>();
       |  chn_tx_flit_map[DAT] = array<uint8_t, FLIT_BUF_SIZE>();
       |  chn_tx_flit_map[SNP] = array<uint8_t, FLIT_BUF_SIZE>();
       |  if(!csn) chn_tx_flit_map[ERQ] = array<uint8_t, FLIT_BUF_SIZE>();
       |  for(auto &[chn, _]: chn_tx_flit_map) tx_fire(chn);
       |  chn_rx_ready_timer_map[REQ] = 0;
       |  chn_rx_ready_timer_map[RSP] = 0;
       |  chn_rx_ready_timer_map[DAT] = 0;
       |  chn_rx_ready_timer_map[SNP] = 0;
       |  if(!csn) chn_rx_ready_timer_map[ERQ] = 0;
       |
       |  chn_tx_valid_timer_map[REQ] = 0;
       |  chn_tx_valid_timer_map[RSP] = 0;
       |  chn_tx_valid_timer_map[DAT] = 0;
       |  chn_tx_valid_timer_map[SNP] = 0;
       |  if(!csn) chn_tx_valid_timer_map[ERQ] = 0;
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
       |    } else if(k == SNP) {
       |      printf("    SNP: ");
       |    } else {
       |      printf("    ERQ: ");
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
       |  if(chn > 5) {
       |    TFS_ERR("illegal channel type %d!\\n", chn);
       |  } else {
       |    chn_rx_ready_timer_map[chn] = dist(random_gen);
       |  }
       |}
       |
       |void NodeManager::tx_fire(uint8_t chn) {
       |  if(chn > 5) {
       |    TFS_ERR("illegal channel type %d!\\n", chn);
       |    return;
       |  }
       |  uint8_t *flit = chn_tx_flit_map[chn].data();
       |  uniform_int_distribution<uint8_t> dist_flit(0, 0xFF);
       |  for(int i = 0; i < FLIT_BUF_SIZE - 1; i++) flit[i] = dist_flit(random_gen);
       |  flit[FLIT_BUF_SIZE - 1] = 0;
       |  uint8_t tgt_pool_size = legal_tgt_pool.at(chn).size();
       |  uniform_int_distribution<uint8_t> dist_tgt_pos(0, tgt_pool_size - 1);
       |  uint8_t pos = dist_tgt_pos(random_gen);
       |  uint16_t tgt_id = legal_tgt_pool.at(chn).at(pos);
       |
       |  uint64_t *head_ptr = (uint64_t *)flit;
       |  head_ptr[0] = clear_field(head_ptr[0], TGT_ID_OFF, NODE_ID_BITS) | (tgt_id << TGT_ID_OFF);
       |  head_ptr[0] = clear_field(head_ptr[0], TXN_ID_OFF, TXN_ID_BITS) | (txn_id << TXN_ID_OFF);
       |  uniform_int_distribution<uint8_t> dist_valid(0, TX_VALID_MAX_DELAY);
       |  chn_tx_valid_timer_map[chn] = dist_valid(random_gen);
       |  txn_id = (txn_id + 1) % (1 << TXN_ID_BITS);
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
       |    if(i == 0) legal_tgt_pool[i][ERQ] = vector<uint16_t>();
       |  }
       |  printf("tfs node manager target:\\n");
       |  for(int i = 0; i < ${allNodeTypeMap.count(_._3 == "LOCAL_TGT_POOL")}; i++) {
       |    const uint8_t node_type = local_node_types[i];
       |    const uint8_t id_num = tfb_get_nodes_size(node_type);
       |    uint16_t *id_arr = new uint16_t[id_num];
       |    tfb_get_nodes(node_type, id_arr);
       |    for(int j = 0; j < id_num; j++) {
       |      const uint16_t node_id = id_arr[j];
       |      for(const auto &chn: type_ejects_map.at(node_type)) {
       |        legal_tgt_pool[LOCAL_TGT_POOL][chn].push_back(node_id);
       |      }
       |      node_mng_pool[node_id] = make_unique<NodeManager>(node_id, node_type);
       |    }
       |    delete[] id_arr;
       |  }
       |  for(int i = 0; i < ${allNodeTypeMap.count(_._3 == "C2C_TGT_POOL")}; i++) {
       |    const uint8_t node_type = csn_node_types[i];
       |    const uint8_t id_num = tfb_get_nodes_size(node_type);
       |    uint16_t *id_arr = new uint16_t[id_num];
       |    tfb_get_nodes(node_type, id_arr);
       |    for(int j = 0; j < id_num; j++) {
       |      const uint16_t node_id = id_arr[j];
       |      for(const auto &chn: type_ejects_map.at(node_type)) {
       |        legal_tgt_pool[C2C_TGT_POOL][chn].push_back(node_id);
       |      }
       |      node_mng_pool[node_id] = make_unique<NodeManager>(node_id, node_type);
       |    }
       |    delete[] id_arr;
       |  }
       |
       |  uint8_t c2c_id_num = tfb_get_nodes_size(C2C_TYPE);
       |  uint16_t *c2c_id_arr = new uint16_t[c2c_id_num];
       |  tfb_get_nodes(C2C_TYPE, c2c_id_arr);
       |  for(uint8_t i = 0; i < c2c_id_num; i++) {
       |    const uint16_t c2c_node_id = c2c_id_arr[i];
       |    const uint16_t chip = get_field(c2c_node_id, 0, NODE_AID_BITS);
       |    const uint16_t net = 1 << (NODE_ID_BITS - 1);
       |    const uint16_t max_nid = 1 << 4;
       |    for(uint16_t nid = 0; nid < max_nid; nid++) {
       |      uint16_t remote_node_id = net | (nid << NODE_AID_BITS) | chip;
       |      legal_tgt_pool[CSN_TGT_POOL][REQ].push_back(remote_node_id);
       |      legal_tgt_pool[CSN_TGT_POOL][RSP].push_back(remote_node_id);
       |      legal_tgt_pool[CSN_TGT_POOL][DAT].push_back(remote_node_id);
       |      legal_tgt_pool[CSN_TGT_POOL][SNP].push_back(remote_node_id);
       |    }
       |    node_mng_pool[c2c_node_id] = make_unique<NodeManager>(c2c_node_id, C2C_TYPE);
       |  }
       |  delete[] c2c_id_arr;
       |  for(const auto &[k, v]: node_mng_pool) v->init();
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
       |    *valid = (mng_ptr->chn_tx_valid_timer_map[chn] == 0) ? 1 : 0;
       |    if(*valid == 1 && ready == 1) mng_ptr->tx_fire(chn);
       |  }
       |  memcpy(flit, tx_flit_ptr, FLIT_BUF_SIZE);
       |}
       |
       |void tfs_get_rx_ready(short int node_id, char chn, svBit valid, svBit *ready, svBit reset) {
       |  const auto mng_ptr = TrafficSim::get_instance().node_mng_pool[node_id].get();
       |  if(reset == 1) {
       |    *ready = 0;
       |  } else {
       |    *ready = (mng_ptr->chn_rx_ready_timer_map[chn] == 0) ? 1 : 0;
       |    if(valid == 1 && *ready == 1) mng_ptr->rx_fire(chn);
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
       |  argparser.add_argument("-v", "--verbose").help("Print verbose information").flag();
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
