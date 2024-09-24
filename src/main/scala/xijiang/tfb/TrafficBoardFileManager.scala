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
      |void tfb_verbose();
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
       |#include <mutex>
       |#include <memory>
       |#include <sstream>
       |#include <iomanip>
       |
       |#define FLIT_SIZE ${params.maxFlitBits}
       |#define TIME_OUT ${params.tfbParams.get.timeOut}
       |#define NODE_NID_BITS ${params.nodeNidBits}
       |#define NODE_TYPE_BITS ${params.nodeTypeBits}
       |#define NODE_NET_BITS ${params.nodeNetBits}
       |#define FLIT_BUF_SIZE ${(params.maxFlitBits + 7) / 8}
       |
       |#define LRF_TYPE ${NodeType.RF | (0 << params.nodeTypeBits)}
       |#define LRI_TYPE ${NodeType.RI | (0 << params.nodeTypeBits)}
       |#define LHF_TYPE ${NodeType.HF | (0 << params.nodeTypeBits)}
       |#define LHI_TYPE ${NodeType.HI | (0 << params.nodeTypeBits)}
       |#define LS_TYPE  ${NodeType.S | (0 << params.nodeTypeBits)}
       |#define CRF_TYPE ${NodeType.RF | (1 << params.nodeTypeBits)}
       |#define CHF_TYPE ${NodeType.HF | (1 << params.nodeTypeBits)}
       |#define C2C_TYPE ${NodeType.C | (1 << params.nodeTypeBits)}
       |
       |#define REQ ${ChannelEncodings.REQ}
       |#define RSP ${ChannelEncodings.RSP}
       |#define DAT ${ChannelEncodings.DAT}
       |#define SNP ${ChannelEncodings.SNP}
       |#define ERQ ${ChannelEncodings.ERQ}
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
       |    TrafficBoard::get_instance().info_lock.lock();                                  \\
       |    fprintf(stderr, "[TFB ERROR] @ %lu: ", TrafficBoard::get_instance().global_timer); \\
       |    fprintf(stderr, __VA_ARGS__);     \\
       |    fflush(stderr);                   \\
       |    TrafficBoard::get_instance().info_lock.unlock();                                 \\
       |  }
       |#define TFB_INFO(...)                 \\
       |  {                                   \\
       |    TrafficBoard::get_instance().info_lock.lock();                                  \\
       |    fprintf(stdout, "[TFB INFO] @ %lu: ", TrafficBoard::get_instance().global_timer); \\
       |    fprintf(stdout, __VA_ARGS__);     \\
       |    fflush(stdout);                   \\
       |    TrafficBoard::get_instance().info_lock.unlock();                                 \\
       |  }
       |
       |
       |using namespace std;
       |
       |inline uint64_t get_field(uint64_t vec, uint8_t offset, uint8_t width) {
       |  return (vec >> offset) & ((1 << width) - 1);
       |}
       |
       |string get_flit_str(const uint8_t *flit) {
       |  ostringstream ss;
       |  ss << hex << "0x";
       |  int i = FLIT_BUF_SIZE;
       |  while(i-- > 0) ss << setw(2) << setfill('0') << (uint16_t)flit[i];
       |  return ss.str();
       |}
       |
       |namespace traffic_board {
       |struct TrafficBoardEntry {
       |  uint64_t inject_time;
       |  uint64_t timer;
       |  uint8_t flit[FLIT_BUF_SIZE];
       |};
       |
       |typedef unordered_map<uint16_t, unordered_map<uint8_t, list<unique_ptr<TrafficBoardEntry>>>> scb_t;
       |typedef unordered_map<uint16_t, unordered_map<uint8_t, unique_ptr<mutex>>> lock_map_t;
       |
       |class TrafficBoard {
       |  private:
       |  TrafficBoard() = default;
       |  // tgtId -> flit_type -> list -> entry
       |  scb_t scoreboard = scb_t();
       |  // tgtId -> flit_type -> list_lock
       |  lock_map_t locks = lock_map_t();
       |  mutex scb_lock;
       |
       |  public:
       |  mutex info_lock;
       |  // local nodes
       |  vector<uint16_t> lrf;
       |  vector<uint16_t> lri;
       |  vector<uint16_t> lhf;
       |  vector<uint16_t> lhi;
       |  vector<uint16_t> lsn;
       |  // csn nodes
       |  vector<uint16_t> crf;
       |  vector<uint16_t> chf;
       |  vector<uint16_t> c2c;
       |  uint64_t global_timer = 0;
       |  bool verbose = false;
       |  TrafficBoard(const TrafficBoard &) = delete;
       |  TrafficBoard &operator=(const TrafficBoard &) = delete;
       |  bool step();
       |  static TrafficBoard &get_instance() {
       |    static TrafficBoard instance;
       |    return instance;
       |  }
       |  void register_node(uint16_t node_id);
       |  svBit add_record(uint16_t node_id, uint8_t chn, const svBitVecVal *flit);
       |  bool match_record(uint16_t node_id, uint8_t chn, const svBitVecVal *flit);
       |};
       |
       |void TrafficBoard::register_node(uint16_t node_id) {
       |  scb_lock.lock();
       |  if(scoreboard.count(node_id) != 0) {
       |    TFB_ERR("cannot register node 0x%x more than once!\\n", node_id);
       |    scb_lock.unlock();
       |    return;
       |  }
       |  uint16_t type = get_field(node_id, NODE_TYPE_OFF, NODE_TYPE_BITS + NODE_NET_BITS);
       |  scoreboard[node_id] = unordered_map<uint8_t, list<unique_ptr<TrafficBoardEntry>>>();
       |  scoreboard[node_id][REQ] = list<unique_ptr<TrafficBoardEntry>>();
       |  scoreboard[node_id][RSP] = list<unique_ptr<TrafficBoardEntry>>();
       |  scoreboard[node_id][DAT] = list<unique_ptr<TrafficBoardEntry>>();
       |  scoreboard[node_id][SNP] = list<unique_ptr<TrafficBoardEntry>>();
       |  scoreboard[node_id][ERQ] = list<unique_ptr<TrafficBoardEntry>>();
       |  locks[node_id] = unordered_map<uint8_t, unique_ptr<mutex>>();
       |  locks[node_id][REQ] = make_unique<mutex>();
       |  locks[node_id][RSP] = make_unique<mutex>();
       |  locks[node_id][DAT] = make_unique<mutex>();
       |  locks[node_id][SNP] = make_unique<mutex>();
       |  locks[node_id][ERQ] = make_unique<mutex>();
       |
       |  switch(type) {
       |    case LRF_TYPE: lrf.push_back(node_id); break;
       |    case LRI_TYPE: lri.push_back(node_id); break;
       |    case LHF_TYPE: lhf.push_back(node_id); break;
       |    case LHI_TYPE: lhi.push_back(node_id); break;
       |    case LS_TYPE:  lsn.push_back(node_id); break;
       |    case CRF_TYPE: crf.push_back(node_id); break;
       |    case CHF_TYPE: chf.push_back(node_id); break;
       |    case C2C_TYPE: c2c.push_back(node_id); break;
       |    default: TFB_ERR("cannot register 0x%x, unknown type tag %d\\n", node_id, type); break;
       |  }
       |  scb_lock.unlock();
       |}
       |
       |#define MONITOR_ERR(cond, ...) \\
       |  if(cond) {                   \\
       |    TFB_ERR(__VA_ARGS__);      \\
       |    return 1;                  \\
       |  }
       |
       |svBit TrafficBoard::add_record(uint16_t node_id, uint8_t chn, const svBitVecVal *flit) {
       |  MONITOR_ERR(scoreboard.count(node_id) == 0, "node 0x%x is sampled before registered!\\n", node_id);
       |  uint16_t tgt_id = get_field(*((const uint64_t *)flit), TGT_ID_OFF, NODE_ID_BITS);
       |  bool c2c = get_field(node_id, NODE_TYPE_OFF, NODE_TYPE_BITS + NODE_NET_BITS) == C2C_TYPE;
       |  bool csn = get_field(node_id, NODE_NET_OFF, NODE_NET_BITS) == 0x1;
       |  uint16_t final_tgt_id = tgt_id;
       |  if(verbose) {
       |    string &&flit_str = get_flit_str((const uint8_t *)flit);
       |    TFB_INFO("node 0x%x inject flit on chn %d tgt_id: 0x%x flit:\\n%s\\n", node_id, chn, tgt_id, flit_str.c_str());
       |  }
       |  if(csn && !c2c) {
       |    uint16_t src_chip = get_field(node_id, 0, NODE_NID_BITS);
       |    uint16_t tgt_chip = get_field(tgt_id, 0, NODE_NID_BITS);
       |    final_tgt_id = (C2C_TYPE << NODE_TYPE_OFF) | tgt_chip;
       |    MONITOR_ERR(src_chip == tgt_chip, "csn node 0x%x injects illegal flit with tgt_id: 0x%x, target chip_id cannot be chip_id of itself!\\n", node_id, tgt_id);
       |  }
       |  MONITOR_ERR(scoreboard.count(final_tgt_id) == 0, "node 0x%x injected flit target node 0x%x is not registered on chn %d!\\n", node_id, final_tgt_id, chn);
       |  auto entry = make_unique<TrafficBoardEntry>();
       |  memcpy(entry->flit, flit, FLIT_BUF_SIZE);
       |  entry->inject_time = global_timer;
       |  entry->timer = 0;
       |  mutex *lock = locks[final_tgt_id][chn].get();
       |  lock_guard lg(*lock);
       |  auto &record_list = scoreboard[final_tgt_id][chn];
       |  record_list.push_back(std::move(entry));
       |  return 0;
       |}
       |
       |bool TrafficBoard::match_record(uint16_t node_id, uint8_t chn, const svBitVecVal *flit) {
       |  bool found = false;
       |  mutex *lock = locks[node_id][chn].get();
       |  lock_guard lg(*lock);
       |  auto &record_list = scoreboard[node_id][chn];
       |  auto pos = record_list.begin();
       |  while(pos != record_list.end()) {
       |    found = (0 == memcmp((*pos)->flit, flit, FLIT_BUF_SIZE));
       |    if(found) break;
       |    pos++;
       |  }
       |  if(found) {
       |    record_list.erase(pos);
       |    if(verbose) {
       |      uint16_t srcId = get_field(*((const uint64_t *)flit), SRC_ID_OFF, NODE_ID_BITS);
       |      uint16_t tgtId = get_field(*((const uint64_t *)flit), TGT_ID_OFF, NODE_ID_BITS);
       |      string &&flit_str = get_flit_str((const uint8_t *)flit);
       |      TFB_INFO("node_id: 0x%x eject flit with srcId: 0x%x tgtId: 0x%x on chn %d flit:\\n%s\\n", node_id, srcId, tgtId, chn, flit_str.c_str());
       |    }
       |  }
       |  return found;
       |}
       |
       |bool TrafficBoard::step() {
       |  bool time_out = false;
       |  global_timer++;
       |  for(auto &[k0, v0]: scoreboard) {
       |    for(auto &[k1, v1]: v0) {
       |      for(auto &d: v1) {
       |        d->timer++;
       |        if(d->timer > TIME_OUT) {
       |          TFB_ERR("node 0x%x chn %d inject time %lu time out!\\n", k0, k1, d->inject_time);
       |          time_out = true;
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
       |  TrafficBoard::get_instance().register_node(node_id);
       |}
       |
       |void tfb_flit_monitor(short int node_id, svBit inject, char flit_type, const svBitVecVal *flit, svBit *fault) {
       |  auto &tfb = TrafficBoard::get_instance();
       |  if(inject) {
       |    *fault = tfb.add_record(node_id, flit_type, flit);
       |  } else {
       |    bool found = tfb.match_record(node_id, flit_type, flit);
       |    uint16_t srcId = get_field(*((const uint64_t *)flit), SRC_ID_OFF, NODE_ID_BITS);
       |    uint16_t tgtId = get_field(*((const uint64_t *)flit), TGT_ID_OFF, NODE_ID_BITS);
       |    if(!found) {
       |      string &&flit_str = get_flit_str((const uint8_t *)flit);
       |      TFB_ERR("node_id: 0x%x eject flit with srcId: 0x%x tgtId: 0x%x not found on chn %d flit:\\n%s\\n", node_id, srcId, tgtId, flit_type, flit_str.c_str());
       |    }
       |    *fault = found ? 0 : 1;
       |  }
       |}
       |
       |uint8_t tfb_step() {
       |  return TrafficBoard::get_instance().step();
       |}
       |
       |uint8_t tfb_get_nodes_size(uint8_t type) {
       |  const auto &tfb = TrafficBoard::get_instance();
       |  switch(type) {
       |    case LRF_TYPE: return tfb.lrf.size();
       |    case LRI_TYPE: return tfb.lri.size();
       |    case LHF_TYPE: return tfb.lhf.size();
       |    case LHI_TYPE: return tfb.lhi.size();
       |    case LS_TYPE:  return tfb.lsn.size();
       |    case CRF_TYPE: return tfb.crf.size();
       |    case CHF_TYPE: return tfb.chf.size();
       |    case C2C_TYPE: return tfb.c2c.size();
       |    default: TFB_ERR("wrong node type 0x%x\\n", type); return 0;
       |  }
       |}
       |
       |uint8_t tfb_get_nodes(uint8_t type, uint16_t *nodes_array_ptr) {
       |  const auto &tfb = TrafficBoard::get_instance();
       |  const vector<uint16_t> *vec_ptr = nullptr;
       |  switch(type) {
       |    case LRF_TYPE: vec_ptr = &(tfb.lrf); break;
       |    case LRI_TYPE: vec_ptr = &(tfb.lri); break;
       |    case LHF_TYPE: vec_ptr = &(tfb.lhf); break;
       |    case LHI_TYPE: vec_ptr = &(tfb.lhi); break;
       |    case LS_TYPE:  vec_ptr = &(tfb.lsn); break;
       |    case CRF_TYPE: vec_ptr = &(tfb.crf); break;
       |    case CHF_TYPE: vec_ptr = &(tfb.chf); break;
       |    case C2C_TYPE: vec_ptr = &(tfb.c2c); break;
       |    default: TFB_ERR("wrong node type 0x%x\\n", type); return 1;
       |  }
       |  for(int i = 0; i < vec_ptr->size(); i++) nodes_array_ptr[i] = (*vec_ptr).at(i);
       |  return 0;
       |}
       |
       |void tfb_verbose() {
       |  TrafficBoard::get_instance().verbose = true;
       |}
       |}""".stripMargin
  }
}
