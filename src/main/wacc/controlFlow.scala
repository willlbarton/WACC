package src.main.wacc

import scala.collection.mutable.ArrayBuffer

case object ControlFlowGraph {
  val nodes: ArrayBuffer[CfgNode] = ArrayBuffer()

  def getNextId = nodes.size + 1
}

case class CfgNode() {
  val id: Int = ControlFlowGraph.getNextId
  // val gen: Set[Int] = Set()
  // val kill: Set[Int] = Set()
  // val reachIn: Set[Int] = Set()
  // val reachOut: Set[Int] = Set()
   
}

