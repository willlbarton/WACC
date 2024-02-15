package src.main.wacc

import scala.collection.mutable.ListBuffer

case class ControlFlowGraph(nodes: ListBuffer[CfgNode] = ListBuffer()) {
  def toList: List[CfgNode] = nodes.toList
  def add(ns: CfgNode*): ControlFlowGraph = {
    this.nodes ++= ns
    this
  }
  def add(ns: List[CfgNode]): ControlFlowGraph = {
    this.nodes ++= ns
    this
  }

  def add(other: ControlFlowGraph): ControlFlowGraph = {
    this.nodes ++= other.nodes
    this
  }
}

case class CfgNode(instruction: Instruction, id: Int) {
  // val gen: Set[Int] = Set()
  // val kill: Set[Int] = Set()
  // val reachIn: Set[Int] = Set()
  // val reachOut: Set[Int] = Set()
}

case object CfgNode {
  private var id = 0
  def apply(instruction: Instruction): CfgNode = {
    id += 1
    CfgNode(instruction, id)
  }
}
