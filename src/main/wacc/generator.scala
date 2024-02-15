package src.main.wacc

import scala.collection.mutable.ListBuffer

object generator {

  val stringLiters: ListBuffer[String] = ListBuffer.empty

  def generate(program: Program, formatter: Formatter): String = genProgram(program).toList
    .map(formatter(_)).mkString("\n") ++ "\n"

  private def genProgram(program: Program): ControlFlowGraph = {
    val graph = ControlFlowGraph().add(
      CfgNode(Directive("globl main")),
      CfgNode(Directive("section .rodata"))
    )
    stringLiters.zipWithIndex.foreach { case (s, i) =>
      graph.add(
        CfgNode(Directive(s"int ${s.length}")),
        CfgNode(Label(s".L.str$i")),
        CfgNode(Directive(s"asciz \"$s\""))
      )
    }
    graph.add(CfgNode(Directive("text")))

    program.functions.foreach(x => graph.add(genFunc(x)))

    graph.add(CfgNode(Label("main")))
    val mainBody = ControlFlowGraph()
    program.body.foreach(x => mainBody.add(genStmt(x)))
    graph.add(genFuncBody(List.empty, mainBody))

    graph.add(CfgNode(Label("_exit")))
    val exitBody = ControlFlowGraph().add(
      // Align stack pointer to 16 bytes
      CfgNode(AndAsm(Immediate(-16), Rsp)),
      CfgNode(CallAsm(Label("exit@plt")))
    )
    graph.add(genFuncBody(List.empty, exitBody))
  }

  private def genFunc(func: Func): ControlFlowGraph = ControlFlowGraph() // TODO

  private def genFuncBody(toSave: List[Reg], body: ControlFlowGraph): ControlFlowGraph = {
    ControlFlowGraph()
     .add(saveRegs(toSave))
     .add(body)
     .add(restoreStack(toSave))
     .add(CfgNode(Ret))
  }

  // save the stack pointer to enter a new scope
  private def saveRegs(regs: List[Reg]): ControlFlowGraph = {
    ControlFlowGraph()
      .add(CfgNode(Push(Rbp))) // Save stack pointer
      .add(regs.map(r => CfgNode(Push(r))))
      .add(CfgNode(Mov(Rbp, Rsp))) // Set stack pointer to base pointer
  }

  // restore the stack pointer to exit a scope
  private def restoreStack(regs: List[Reg]): ControlFlowGraph = {
    ControlFlowGraph()
      .add(regs.reverseIterator.map(r => CfgNode(Pop(r))).toList)
      .add(CfgNode(Pop(Rbp)))
  }

  private def genStmt(stmt: Stmt): ControlFlowGraph =
    stmt match {
      case Skip => ControlFlowGraph()
      case Exit(expr) => genExit(expr)
      case Return(expr) => ControlFlowGraph().add(genExpr(expr)).add(CfgNode(Ret))
      case _ => ControlFlowGraph() // TODO
    }

  private def genExit(expr: Expr): ControlFlowGraph = {
    ControlFlowGraph()
     .add(genExpr(expr))
     .add(
      CfgNode(Mov(Rax, Rdi)),
      CfgNode(CallAsm(Label("_exit"))),
      CfgNode(Mov(Immediate(0), Rax))
    )
  }

  private def genExpr(expr: Expr): ControlFlowGraph = ControlFlowGraph() // TODO
}