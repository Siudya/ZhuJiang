package xijiang.tfs

import org.chipsalliance.cde.config.Parameters
import xijiang.NodeType
import xs.utils.FileRegisters
import zhujiang.ZJParametersKey
import zhujiang.chi.ChannelEncodings

case class TrafficSimParams(
  rxReadyMaxDelay: Int = 10
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
       |    fprintf(stderr, "[TFB ERROR]: "); \\
       |    fprintf(stderr, __VA_ARGS__);     \\
       |  }
       |
       |using namespace std;
       |
       |class NodeManager {
       |  public:
       |  uint32_t ready_timers[4];
       |  uint16_t node_id;
       |  uint16_t pool_type = 0;
       |  unordered_map<uint8_t, vector<uint16_t>> legal_tgt_pool;
       |  NodeManager(uint16_t node_id);
       |  void gen_tx_flit(uint8_t *flit, uint8_t chn);
       |  bool gen_rx_ready(uint8_t chn);
       |  void step();
       |  void fire(uint8_t chn);
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
       |
       |  mt19937 random_gen;
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
       |NodeManager::NodeManager(uint16_t node_id) {
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
       |  memset(ready_timers, 0, 4 * sizeof(uint32_t));
       |
       |  for(const auto &[k, v] : tfs.legal_tgt_pool[pool_type]) {
       |    legal_tgt_pool[k] = vector<uint16_t>();
       |    for(const auto &tgt : v) {
       |      if(tgt != node_id) legal_tgt_pool[k].push_back(tgt);
       |    }
       |  }
       |}
       |
       |bool NodeManager::gen_rx_ready(uint8_t chn) {
       |  auto &tfs = TrafficSim::get_instance();
       |  if(chn > 4) {
       |    TFS_ERR("illegal channel type %d! @ time %ld\\n", chn, tfs.global_timer);
       |    return false;
       |  } else {
       |    return ready_timers[chn] == 0;
       |  }
       |}
       |
       |void NodeManager::fire(uint8_t chn) {
       |  auto &tfs = TrafficSim::get_instance();
       |  uniform_int_distribution<uint8_t> dist(0, RX_READY_MAX_DELAY);
       |  if(chn > 4) {
       |    TFS_ERR("illegal channel type %d! @ time %ld\\n", chn, tfs.global_timer);
       |  } else {
       |    ready_timers[chn] = dist(tfs.random_gen);
       |  }
       |}
       |
       |void NodeManager::gen_tx_flit(uint8_t *flit, uint8_t chn) {
       |  auto &tfs = TrafficSim::get_instance();
       |  uniform_int_distribution<uint8_t> dist_flit(0, 0xFF);
       |  for(int i = 0; i < FLIT_BUF_SIZE; i++) flit[i] = dist_flit(tfs.random_gen);
       |
       |  uint8_t tgt_pool_size = legal_tgt_pool[chn].size();
       |  uniform_int_distribution<uint8_t> dist_tgt_pos(0, tgt_pool_size - 1);
       |  uint8_t pos = dist_tgt_pos(tfs.random_gen);
       |  uint16_t tgt_id = legal_tgt_pool[chn].at(pos);
       |
       |  uint64_t *head_ptr = (uint64_t *)flit;
       |  head_ptr[0] = clear_field(head_ptr[0], TGT_ID_OFF, NODE_ID_BITS) | (tgt_id << TGT_ID_OFF);
       |}
       |
       |void NodeManager::step() {
       |  for(int i = 0; i < 4; i++) {
       |    ready_timers[i] = ready_timers[i] == 0 ? ready_timers[i] : (ready_timers[i] - 1);
       |  }
       |}
       |
       |void TrafficSim::init() {
       |  if(initialized) return;
       |  for(int i = 0; i < 3; i++) {
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
       |
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
       |  random_gen.seed(seed);
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
       |  const auto mng_ptr = TrafficSim::get_instance().node_mng_pool[node_id].get();
       |  if(reset) {
       |    *valid = 0;
       |  } else {
       |    *valid = 1;
       |    if(ready == 1) mng_ptr->gen_tx_flit((uint8_t *)flit, chn);
       |  }
       |}
       |
       |void tfs_get_rx_ready(short int node_id, char chn, svBit valid, svBit *ready, svBit reset) {
       |  const auto mng_ptr = TrafficSim::get_instance().node_mng_pool[node_id].get();
       |  if(reset) {
       |    *ready = 0;
       |  } else {
       |    if(valid == 1 && *ready == 1) mng_ptr->fire(chn);
       |    *ready = mng_ptr->gen_rx_ready(chn) ? 1 : 0;
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
       |add_requires("verilator")
       |
       |target("VeriRing")
       |  set_toolchains("@verilator")
       |  add_rules("verilator.static")
       |  add_includedirs("env/tfb/include")
       |  add_values("verilator.flags", "--top-module", "TrafficSimTop", "--trace")
       |  add_values("verilator.flags", "--no-timing", "--threads", "8", "--threads-dpi", "all")
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
       |  add_deps("VeriRing")""".stripMargin

  def mainStr: String =
    s"""
       |#include <iostream>
       |using namespace std;
       |int main() {
       |  cout << "Hello World!" << endl;
       |}
       |""".stripMargin
}
