package xijiang.tfb

import org.chipsalliance.cde.config.Parameters
import xijiang.NodeType
import xs.utils.FileRegisters
import zhujiang.ZJParametersKey
import zhujiang.chi.ChannelEncodings

case class TrafficBoardParams(
  timeOut: Int = 500
)

object TrafficBoardFileManager {
  def release(p: Parameters) = {
    FileRegisters.add("env/tfb/include", "traffic_board.h", header, true)
    FileRegisters.add("env/tfb/src", "traffic_board.cpp", source(p), true)
  }

  def header: String =
    """
      |#ifndef __TRAFFIC_BOARD_H__
      |#define __TRAFFIC_BOARD_H__
      |#include "svdpi.h"
      |
      |#ifdef __cplusplus
      |extern "C" {
      |#endif
      |
      |void tfb_register_node(short int node_id);
      |
      |void tfb_flit_monitor(short int node_id, svBit inject, char flit_type, const svBitVecVal *flit, svBit *fault);
      |
      |uint8_t tfb_step();
      |
      |uint8_t tfb_get_nodes_size(uint8_t type);
      |
      |uint8_t tfb_get_nodes(uint8_t type, uint16_t *nodes_array_ptr);
      |
      |#ifdef __cplusplus
      |}
      |#endif
      |
      |#endif //__TRAFFIC_BOARD_H__
      |""".stripMargin

  def source(p: Parameters) = {
    val params = p(ZJParametersKey)
    s"""
       |#include "svdpi.h"
       |#include <cstdint>
       |#include <cstdio>
       |#include <cstring>
       |#include <list>
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
       |#define NODE_TYPE_OFF NODE_NID_BITS
       |#define NODE_NET_OFF (NODE_NID_BITS + NODE_TYPE_BITS)
       |
       |#define NODE_ID_BITS (NODE_NID_BITS + NODE_TYPE_BITS + NODE_NET_BITS)
       |#define TGT_ID_OFF 4
       |#define SRC_ID_OFF (TGT_ID_OFF + NODE_ID_BITS)
       |
       |#define TFB_ERR(...)                  \\
       |  {                                   \\
       |    fprintf(stderr, "[TFB ERROR]: "); \\
       |    fprintf(stderr, __VA_ARGS__);     \\
       |  }
       |
       |inline uint64_t get_field(uint64_t vec, uint8_t offset, uint8_t width) {
       |  return (vec >> offset) & ((1 << width) - 1);
       |}
       |
       |using namespace std;
       |
       |namespace traffic_board {
       |struct TrafficBoardEntry {
       |  uint64_t inject_time;
       |  uint64_t timer;
       |  uint8_t flit[FLIT_BUF_SIZE];
       |};
       |
       |typedef unordered_map<uint16_t, unordered_map<uint8_t, list<TrafficBoardEntry>>> scb_t;
       |
       |class TrafficBoard {
       |  private:
       |  TrafficBoard() = default;
       |
       |  public:
       |  // tgtId -> flit_type -> entry
       |  scb_t scoreboard = scb_t();
       |  // local nodes
       |  vector<uint16_t> lrn;
       |  vector<uint16_t> lhf;
       |  vector<uint16_t> lhi;
       |  // csn nodes
       |  vector<uint16_t> crn;
       |  vector<uint16_t> chf;
       |  vector<uint16_t> c2c;
       |  uint64_t global_timer = 0;
       |  bool fault = false;
       |  TrafficBoard(const TrafficBoard &) = delete;
       |  TrafficBoard &operator=(const TrafficBoard &) = delete;
       |  bool step();
       |  bool find_record(uint16_t node_id, uint8_t flit_type, const uint8_t *flit);
       |  static TrafficBoard &get_instance() {
       |    static TrafficBoard instance;
       |    return instance;
       |  }
       |};
       |
       |bool TrafficBoard::find_record(uint16_t node_id, uint8_t flit_type, const uint8_t *flit) {
       |  bool found = false;
       |  auto &recore_list = scoreboard[node_id][flit_type];
       |  auto pos = recore_list.begin();
       |  while(pos != recore_list.end()) {
       |    found = (0 == memcmp((*pos).flit, flit, FLIT_BUF_SIZE));
       |    if(found) break;
       |    pos++;
       |  }
       |  if(found) recore_list.erase(pos);
       |  return found;
       |}
       |
       |bool TrafficBoard::step() {
       |  bool time_out = false;
       |  global_timer++;
       |  for(auto &[node_id, nodeMsgs] : scoreboard) {
       |    for(auto &[channel, flit_list] : nodeMsgs) {
       |      for(auto &entry : flit_list) {
       |        entry.timer++;
       |        time_out = entry.timer > TIME_OUT;
       |        if(time_out) {
       |          auto flit = *((const uint64_t *)entry.flit);
       |          uint16_t srcId = get_field(flit, SRC_ID_OFF, NODE_ID_BITS);
       |          uint16_t tgtId = get_field(flit, TGT_ID_OFF, NODE_ID_BITS);
       |          TFB_ERR("FLIT Time out! inject time: %lu, srcId: 0x%x tgtId: 0x%x\\n", entry.inject_time, srcId, tgtId);
       |        }
       |      }
       |    }
       |  }
       |  return time_out;
       |}
       |}// namespace traffic_board
       |
       |using namespace traffic_board;
       |extern "C" {
       |void tfb_register_node(short int node_id) {
       |  auto &tfb = TrafficBoard::get_instance();
       |  uint8_t type = get_field(node_id, NODE_TYPE_OFF, NODE_TYPE_BITS);
       |  uint8_t net = get_field(node_id, NODE_NET_OFF, NODE_NET_BITS);
       |  if(tfb.scoreboard.count(node_id)) TFB_ERR("cannot register node 0x%x more than once!\\n", node_id);
       |  tfb.scoreboard[node_id] = unordered_map<uint8_t, list<TrafficBoardEntry>>();
       |  tfb.scoreboard[node_id][REQ] = list<TrafficBoardEntry>();
       |  tfb.scoreboard[node_id][RSP] = list<TrafficBoardEntry>();
       |  tfb.scoreboard[node_id][DAT] = list<TrafficBoardEntry>();
       |  tfb.scoreboard[node_id][SNP] = list<TrafficBoardEntry>();
       |
       |  if(0 == net) {
       |    if(type == R_TYPE)
       |      tfb.lrn.push_back(node_id);
       |    else if(type == HF_TYPE)
       |      tfb.lhf.push_back(node_id);
       |    else if(type == HI_TYPE)
       |      tfb.lhi.push_back(node_id);
       |    else
       |      TFB_ERR("cannot register C2C 0x%x on local ring\\n", node_id)
       |  } else {
       |    if(type == R_TYPE)
       |      tfb.crn.push_back(node_id);
       |    else if(type == HF_TYPE)
       |      tfb.chf.push_back(node_id);
       |    else if(type == C_TYPE)
       |      tfb.c2c.push_back(node_id);
       |    else
       |      TFB_ERR("cannot register HNI 0x%x on csn ring\\n", node_id);
       |  }
       |}
       |
       |#define MONITOR_ERR(cond, ...) if(cond) { \\
       |  TFB_ERR(__VA_ARGS__); \\
       |  *fault = true; \\
       |  return; \\
       |}
       |
       |void tfb_flit_monitor(short int node_id, svBit inject, char flit_type, const svBitVecVal *flit, svBit *fault) {
       |  *fault = false;
       |  auto &tfb = TrafficBoard::get_instance();
       |  MONITOR_ERR(tfb.scoreboard.count(node_id) == 0, "node 0x%x is sampled before registered!\\n", node_id);
       |
       |  if(inject) {
       |    uint16_t tgt_id = get_field(*((const uint64_t *)flit), TGT_ID_OFF, NODE_ID_BITS);
       |    bool c2c = get_field(node_id, NODE_TYPE_OFF, NODE_TYPE_BITS) == C_TYPE;
       |    bool csn = get_field(node_id, NODE_NET_OFF, NODE_NET_BITS) == 0x1;
       |    uint16_t final_tgt_id = tgt_id;
       |    if(csn && !c2c){
       |      uint16_t src_chip = get_field(node_id, 0, NODE_NID_BITS);
       |      uint16_t tgt_chip = get_field(tgt_id, 0, NODE_NID_BITS);
       |      final_tgt_id = (1 << NODE_NET_OFF) | (C_TYPE << NODE_TYPE_OFF) | tgt_chip;
       |      MONITOR_ERR(src_chip == tgt_chip, "csn node 0x%x injects illegal flit with tgt_id: 0x%x, target chip_id cannot be chip_id of itself!\\n", node_id, tgt_id);
       |    }
       |    MONITOR_ERR(tfb.scoreboard.count(final_tgt_id) == 0, "flit target node 0x%x is not registered!\\n", final_tgt_id);
       |    TrafficBoardEntry entry;
       |    memcpy(entry.flit, flit, FLIT_BUF_SIZE);
       |    entry.inject_time = tfb.global_timer;
       |    entry.timer = 0;
       |    tfb.scoreboard[final_tgt_id][(uint8_t)flit_type].push_back(entry);
       |  } else {
       |    bool not_found = !tfb.find_record(node_id, flit_type, (const uint8_t *)flit);
       |    MONITOR_ERR(not_found, "eject record not found! node_id: 0x%x, flit_type: 0x%x\\n", node_id, flit_type);
       |  }
       |}
       |
       |uint8_t tfb_step() {
       |  return TrafficBoard::get_instance().step();
       |}
       |
       |uint8_t tfb_get_nodes_size(uint8_t type) {
       |  const auto &tfb = TrafficBoard::get_instance();
       |  if(type == 0x00)
       |    return tfb.lrn.size();
       |  else if(type == 0x01)
       |    return tfb.lhf.size();
       |  else if(type == 0x02)
       |    return tfb.lhi.size();
       |  else if(type == 0x10)
       |    return tfb.crn.size();
       |  else if(type == 0x11)
       |    return tfb.chf.size();
       |  else if(type == 0x13)
       |    return tfb.c2c.size();
       |  else
       |    TFB_ERR("wrong node type 0x%x\\n", type);
       |  return 0;
       |}
       |
       |uint8_t tfb_get_nodes(uint8_t type, uint16_t *nodes_array_ptr) {
       |  const auto &tfb = TrafficBoard::get_instance();
       |  const vector<uint16_t> *vec_ptr = nullptr;
       |  if(type == 0x00)
       |    vec_ptr = &(tfb.lrn);
       |  else if(type == 0x01)
       |    vec_ptr = &(tfb.lhf);
       |  else if(type == 0x02)
       |    vec_ptr = &(tfb.lhi);
       |  else if(type == 0x10)
       |    vec_ptr = &(tfb.crn);
       |  else if(type == 0x11)
       |    vec_ptr = &(tfb.chf);
       |  else if(type == 0x13)
       |    vec_ptr = &(tfb.c2c);
       |  else {
       |    TFB_ERR("wrong node type 0x%x\\n", type);
       |    return 1;
       |  }
       |  int i = 0;
       |  for(const auto &id : *vec_ptr) {
       |    nodes_array_ptr[i] = id;
       |    i++;
       |  }
       |  return 0;
       |}
       |
       |}""".stripMargin
  }
}
