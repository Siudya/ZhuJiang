package xijiang.router

import org.chipsalliance.cde.config.Parameters
import xijiang.Node

class PipelineRouter(node: Node)(implicit p: Parameters) extends BaseRouter(node, Seq(), Seq()) {
  router.nodeId := node.nodeId
}
