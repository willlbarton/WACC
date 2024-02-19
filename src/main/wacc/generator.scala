package src.main.wacc

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object generator {

  val stringLiters: mutable.Map[String, Int] = mutable.Map.empty

  def generate(program: Program, formatter: Formatter): String = genProgram(program)
    .map(formatter(_)).mkString("\n")

  private def genProgram(program: Program): List[Instruction] = {
    val graph: ListBuffer[Instruction] = ListBuffer(
      Directive("globl main"),
      Directive("section .rodata")
    )
    stringLiters.foreach { case (s, i) =>
      graph += Directive(s"int ${s.length}")
      graph += Label(s".L.str$i")
      graph += Directive(s"asciz \"$s\"")
    }
    graph += Directive("text")

    program.functions.foreach(x => graph ++= genFunc(x, SymbolTable(None)))

    graph.addOne(Label("main"))
    val mainBody: ListBuffer[Instruction] = ListBuffer()
    val mainSymTable: SymbolTable[Dest] = SymbolTable(None)
    program.body.foreach(x => mainBody ++= genStmt(x, mainSymTable, freeRegs))
    graph ++= genFuncBody(List.empty, mainBody)

    graph.addOne(Label("_exit"))
    val exitBody: ListBuffer[Instruction] = ListBuffer(
      // Align stack pointer to 16 bytes
      AndAsm(Rsp, Immediate(-16)),
      CallAsm(Label("exit@plt"))
    )
    graph ++= genFuncBody(List.empty, exitBody)

    stringLiters.keys.foreach(s => graph ++= genPrintLiteral(s))

    graph.toList
  }

  private def genFunc(func: Func, symTable: SymbolTable[Dest]): ListBuffer[Instruction] = ListBuffer.empty // TODO

  private def genFuncBody(toSave: List[Reg], body: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    val is: ListBuffer[Instruction] = ListBuffer()
    is ++= saveRegs(toSave)
    is += body
    is += restoreRegs(toSave)
    is += Ret
    body
  }

  // save the stack pointer to enter a new scope
  private def saveRegs(regs: List[Reg]): ListBuffer[Instruction] = {
    val is: ListBuffer[Instruction] = ListBuffer()
    is += Push(Rbp)
    is ++= regs.map(r => Push(r))
    is += Mov(Rsp, Rbp) // Set stack pointer to base pointer
  }

  // restore the stack pointer to exit a scope
  private def restoreRegs(regs: List[Reg]): ListBuffer[Instruction] = {
    val is: ListBuffer[Instruction] = ListBuffer()
    is += Mov(Rbp, Rsp)
    is ++= regs.map(r => Pop(r))
    is += Pop(Rbp)
  }

  private def genStmt(stmt: Stmt, symTable: SymbolTable[Dest], freeRegs: List[Dest]): ListBuffer[Instruction] =
    stmt match {
      case Skip => ListBuffer[Instruction]()
      case Exit(expr) => genExit(expr, symTable)
      case Return(expr) => ListBuffer().addAll(genExpr(expr, symTable, freeRegs)).addOne(Ret)
      case _ => ListBuffer[Instruction]() // TODO
    }

  private def genExit(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] = {
    val is: ListBuffer[Instruction] = ListBuffer()
    is ++= genExpr(expr, symTable, freeRegs)
    is += Mov(Rax, Rdi)
    is += CallAsm(Label("_exit"))
    is += Mov(Rax, Immediate(0))
    is
  }

  private def genExpr(expr: Expr, symTable: SymbolTable[Dest], freeRegs: List[Dest]): ListBuffer[Instruction] = ListBuffer[Instruction]() // TODO

  private def genPrintLiteral(s: String): ListBuffer[Instruction] = {
    val id: Int = stringLiters(s)
    val graph: ListBuffer[Instruction] = ListBuffer(Label(s"_print$id"))
    val printBody: ListBuffer[Instruction] = ListBuffer()
    printBody += AndAsm(Rsp, Immediate(-16))
    printBody += Mov(Rdi, Rdx)
    printBody += Mov(Address(Immediate(-4), Rdi), Rsi)
    printBody += Lea(Rdi, Address(Label(s".L.str$id"), Rsi))
    printBody += Mov(Rax, Immediate(0))
    printBody += CallAsm(Label("printf@plt"))
    printBody += Mov(Rdi, Immediate(0))
    printBody += CallAsm(Label("fflush@plt"))
    graph ++= genFuncBody(List.empty, printBody)
  }
}