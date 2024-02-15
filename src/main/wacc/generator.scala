package src.main.wacc

object generator {

  def generate(program: Program, formatter: Formatter): String = genProgram(program).toList
    .map(formatter(_)).mkString("\n")

  private def genProgram(program: Program): ControlFlowGraph = {
    val graph = ControlFlowGraph()
    program.functions.foreach(x => graph.add(genFunc(x)))

    graph.add(CfgNode(Label("main")))
    val mainBody = ControlFlowGraph()
    program.body.foreach(x => mainBody.add(genStmt(x)))
    graph.add(genFuncBody(List.empty, mainBody))

    graph.add(CfgNode(Label("_exit")))
    val exitBody = ControlFlowGraph()
    exitBody.add(
      // Align stack pointer to 16 bytes
      CfgNode(AndAsm(Immediate(-16), Register(Rsp))),
      CfgNode(CallAsm(Label("exit@plt")))
    )
    graph.add(exitBody)

    graph
  }

  private def genFunc(func: Func): ControlFlowGraph = ???

  private def genFuncBody(toSave: List[Register], body: ControlFlowGraph): ControlFlowGraph = {
    val graph = ControlFlowGraph()
    graph.add(saveRegs(toSave))
    graph.add(body)
    graph.add(restoreStack(toSave))
    graph.add(CfgNode(Ret))
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


  private def genStmt(stmt: Stmt): ControlFlowGraph =
    stmt match {
      case Skip => ControlFlowGraph()
      case Exit(expr) => genExit(expr)
    }

  private def genExit(expr: Expr): ControlFlowGraph = {
    val graph = ControlFlowGraph()
    graph.add(genExpr(expr))
    graph.add(
      CfgNode(Mov(Register(Rax), Register(Rdi))),
      CfgNode(CallAsm(Label("_exit"))),
      CfgNode(Mov(Immediate(0), Register(Rax)))
    )
    graph
  }

  private def genExpr(expr: Expr): ControlFlowGraph = ???
}