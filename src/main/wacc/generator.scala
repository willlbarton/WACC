package src.main.wacc

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object generator {

  val stringLiters: mutable.Map[String, Int] = mutable.Map.empty
  private val paramRegs: List[Reg] = List(Edi(), Esi(), Edx(), Ecx(), R8(), R9())
  private val nonParamRegs: List[Reg] = List(Ebx(), R10(), R11(), R12(), R13(), R14(), R15())

  def generate(program: Program, formatter: Formatter): String = genProgram(program)
    .map(formatter(_))
    .mkString("\n") ++ "\n"

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
    val freeRegs: ListBuffer[Dest] = ListBuffer.from(nonParamRegs)
    program.body.foreach(x => mainBody ++= genStmt(x, mainSymTable, freeRegs))
    graph ++= genFuncBody(List.empty, mainBody)

    graph.addOne(Label("_exit"))
    val exitBody: ListBuffer[Instruction] = ListBuffer(
      // Align stack pointer to 16 bytes
      AndAsm(Rsp, Immediate(-16)),
      CallAsm(Label("exit@plt"))
    )
    graph ++= genFuncBody(List.empty, exitBody)

    graph ++= genPrint(stringType, "%.*s")
    graph ++= genPrint(intType, "%d")
    graph ++= genPrint(printlnType, "")

    graph.toList
  }

  private def genFunc(func: Func, symTable: SymbolTable[Dest]): ListBuffer[Instruction] = ListBuffer.empty // TODO

  private def genFuncBody(toSave: List[Reg], body: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    val is: ListBuffer[Instruction] = ListBuffer()
    is ++= saveRegs(toSave)
    is ++= body
    is ++= restoreRegs(toSave)
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

  private def genStmt(
      stmt: Stmt,
      symTable: SymbolTable[Dest],
      freeRegs: ListBuffer[Dest]
  ): ListBuffer[Instruction] =
    stmt match {
      case Skip         => ListBuffer()
      case Exit(expr)   => genExit(expr, symTable)
      case Return(expr) => ListBuffer().addAll(genExpr(expr, symTable, freeRegs)).addOne(Ret)
      case _            => ListBuffer() // TODO
    }

  private def genExpr(
      expr: Expr,
      symTable: SymbolTable[Dest],
      freeRegs: ListBuffer[Dest]
  ): ListBuffer[Instruction] = ListBuffer() // TODO

  private def genExit(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] = {
    val freeRegs: ListBuffer[Dest] = ListBuffer.from(nonParamRegs)
    val is: ListBuffer[Instruction] = ListBuffer()
    is ++= genExpr(expr, symTable, freeRegs)
    is += Mov(Eax(), Edi())
    is += CallAsm(Label("_exit"))
    is += Mov(Eax(), Immediate(0))
    is
  }

  private lazy val stringType = 's'
  private lazy val intType = 'i'
  private lazy val printlnType = 'n'
  private def genPrint(typ: Char, format: String): ListBuffer[Instruction] = {
    val graph: ListBuffer[Instruction] = ListBuffer()
    graph += Directive("section .rodata")
    graph += Directive(s"int ${format.length}")
    graph += Label(s".print${typ}_format")
    graph += Directive(s"asciz \"$format\"")
    graph += Label(s"_print$typ")
    val printBody: ListBuffer[Instruction] = ListBuffer()
    printBody += AndAsm(Rsp, Immediate(-16))

    if (typ == stringType) {
      printBody += Mov(Edi(Size64), Edx(Size64))
      printBody += Mov(Address(Immediate(-4), Edi(Size64)), Esi())
    } else if (typ == intType) {
      printBody += Mov(Edi(), Esi())
    }

    printBody += Lea(Edi(Size64), Address(Label(s".print${typ}_format"), Rip))
    printBody += Mov(Eax(Size8), Immediate(0))

    if (typ == printlnType) {
      printBody += CallAsm(Label("puts@plt"))
    } else {
      printBody += CallAsm(Label("printf@plt"))
    }


    printBody += Mov(Edi(Size64), Immediate(0))
    printBody += CallAsm(Label("fflush@plt"))
    graph ++= genFuncBody(List.empty, printBody)
  }
}
