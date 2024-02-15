package src.main.wacc

object generator {

  def generate(program: Program, formatter: Formatter): String = genProgram(program).toList
    .map(formatter(_)).mkString("\n")

  private def genProgram(program: Program): ControlFlowGraph = {
    val graph = ControlFlowGraph()
    program.functions.foreach(x => graph.add(genFunc(x)))
    graph.add(saveRegs(List.empty))
    program.body.foreach(x => graph.add(genStmt(x)))
    graph.add(restoreStack(List.empty))
    graph
  }

  // save the stack pointer to enter a new scope
  private def saveRegs(regs: List[Register]): ControlFlowGraph = {
    val graph = ControlFlowGraph()
    graph.add(CfgNode(Push(Register(Rbp)))) // Save stack pointer
    graph.add(regs.map(r => CfgNode(Push(r))))
    graph.add(CfgNode(Mov(Register(Rbp), Register(Rsp)))) // Set stack pointer to base pointer
    graph
  }

  // restore the stack pointer to exit a scope
  private def restoreStack(regs: List[Register]): ControlFlowGraph = {
    val graph = ControlFlowGraph()
    graph.add(regs.reverseIterator.map(r => CfgNode(Pop(r))).toList)
    graph.add(CfgNode(Pop(Register(Rbp))))
    graph
  }

  private def genFunc(func: Func): ControlFlowGraph = ???

  private def genStmt(stmt: Stmt): ControlFlowGraph =
    stmt match {
      case Skip => ControlFlowGraph()
      case Exit(expr) => genExit(expr)
    }

  private def genExit(expr: Expr): ControlFlowGraph = ???

  private def genExpr(expr: Expr): ControlFlowGraph = ???
}