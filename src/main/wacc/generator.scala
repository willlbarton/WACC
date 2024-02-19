package src.main.wacc

import scala.collection.mutable

object generator {

  val stringLiters: mutable.Map[String, Int] = mutable.Map.empty

  def generate(program: Program, formatter: Formatter): String = genProgram(program).toList
    .map(formatter(_)).mkString("\n")

  private def genProgram(program: Program): ControlFlowGraph = {
    val graph = ControlFlowGraph().add(
      CfgNode(Directive("globl main")),
      CfgNode(Directive("section .rodata"))
    )
    stringLiters.foreach { case (s, i) =>
      graph.add(
        CfgNode(Directive(s"int ${s.length}")),
        CfgNode(Label(s".L.str$i")),
        CfgNode(Directive(s"asciz \"$s\""))
      )
    }
    graph.add(CfgNode(Directive("text")))

    program.functions.foreach(x => graph.add(genFunc(x, SymbolTable(None))))

    graph.add(CfgNode(Label("main")))
    val mainBody = ControlFlowGraph()
    val mainSymTable: SymbolTable[Temporary] = SymbolTable(None)
    val minTemporary = NonParamTemp(0)
    program.body.foreach(x => mainBody.add(genStmt(x, mainSymTable, minTemporary)))
    graph.add(genFuncBody(List.empty, mainBody))

    graph.add(CfgNode(Label("_exit")))
    val exitBody = ControlFlowGraph().add(
      // Align stack pointer to 16 bytes
      CfgNode(AndAsm(Rsp, Immediate(-16))),
      CfgNode(CallAsm(Label("exit@plt")))
    )
    graph.add(genFuncBody(List.empty, exitBody))

    stringLiters.keys.foreach(s => graph.add(genPrintLiteral(s)))

    graph
  }

  private def genFunc(func: Func, symTable: SymbolTable[Temporary]): ControlFlowGraph = ControlFlowGraph() // TODO

  private def genFuncBody(toSave: List[Reg], body: ControlFlowGraph): ControlFlowGraph = {
    ControlFlowGraph()
     .add(saveRegs(toSave))
     .add(body)
     .add(restoreRegs(toSave))
     .add(CfgNode(Ret))
  }

  // save the stack pointer to enter a new scope
  private def saveRegs(regs: List[Reg]): ControlFlowGraph = {
    ControlFlowGraph()
      .add(CfgNode(Push(Rbp))) // Save stack pointer
      .add(regs.map(r => CfgNode(Push(r))))
      .add(CfgNode(Mov(Rsp, Rbp))) // Set stack pointer to base pointer
  }

  // restore the stack pointer to exit a scope
  private def restoreRegs(regs: List[Reg]): ControlFlowGraph = {
    ControlFlowGraph()
      .add(CfgNode(Mov(Rbp, Rsp)))
      .add(regs.reverseIterator.map(r => CfgNode(Pop(r))).toList)
      .add(CfgNode(Pop(Rbp)))
  }

  private def genStmt(stmt: Stmt, symTable: SymbolTable[Temporary], minTemporary: Temporary): ControlFlowGraph =
    stmt match {
      case Skip => ControlFlowGraph()
      case Exit(expr) => genExit(expr, symTable)
      case Return(expr) => ControlFlowGraph().add(genExpr(expr, symTable, minTemporary)).add(CfgNode(Ret))
      case _ => ControlFlowGraph() // TODO
    }

  private def genExit(expr: Expr, symTable: SymbolTable[Temporary]): ControlFlowGraph = {
    ControlFlowGraph()
     .add(genExpr(expr, symTable, NonParamTemp(0)))
     .add(
      CfgNode(Mov(Rax, Rdi)),
      CfgNode(CallAsm(Label("_exit"))),
      CfgNode(Mov(Rax, Immediate(0)))
    )
  }

  private def genExpr(expr: Expr, symTable: SymbolTable[Temporary], minTemporary: Temporary): ControlFlowGraph = ControlFlowGraph() // TODO

  private def genPrintLiteral(s: String): ControlFlowGraph = {
    val id: Int = stringLiters(s)
    val graph = ControlFlowGraph().add(CfgNode(Label(s"_print$id")))
    val printBody = ControlFlowGraph().add(
      CfgNode(AndAsm(Rsp, Immediate(-16))),
      CfgNode(Mov(Rdi, Rdx)),
      CfgNode(Mov(Address(Immediate(-4), Rdi), Rsi)),
      CfgNode(Lea(Rdi, Address(Label(s".L.str$id"), Rsi))),
      CfgNode(Mov(Rax, Immediate(0))),
      CfgNode(CallAsm(Label("printf@plt"))),
      CfgNode(Mov(Rdi, Immediate(0))),
      CfgNode(CallAsm(Label("fflush@plt")))
    )
    graph.add(genFuncBody(List.empty, printBody))
  }
}