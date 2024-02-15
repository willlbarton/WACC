package src.main.wacc

import scala.collection.mutable.ListBuffer

case class ControlFlowGraph(nodes: ListBuffer[CfgNode] = ListBuffer()) {
  def addNode(instruction: Instruction): Unit = nodes += CfgNode(instruction, nodes.length)
  def toList: List[CfgNode] = nodes.toList
}

case class CfgNode(instruction: Instruction, id: Int) {
  // val gen: Set[Int] = Set()
  // val kill: Set[Int] = Set()
  // val reachIn: Set[Int] = Set()
  // val reachOut: Set[Int] = Set()
}
