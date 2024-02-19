package src.main.wacc

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object generator {

  def lb(instructions: Any*): ListBuffer[Instruction] = {
    val listBuffer = ListBuffer[Instruction]()

    for (instruction <- instructions) {
      instruction match {
        case i: Instruction                => listBuffer += i
        case list: ListBuffer[Instruction] => listBuffer ++= list
        case _ => // Ignore other types, you may choose to handle them differently
      }
    }

    listBuffer
  }

  val stringLiters: mutable.Map[String, Int] = mutable.Map.empty

  def generate(program: Program, formatter: Formatter): String = genProgram(program)
    .map(formatter(_))
    .mkString("\n")

  private def genProgram(program: Program): List[Instruction] = {
    val instructions = lb(
      Directive("globl main"),
      Directive("section .rodata"),
      stringLiters.foreach { case (s, i) =>
        lb(
          Directive(s"int ${s.length}"),
          Label(s".L.str$i"),
          Directive(s"asciz \"$s\"")
        )
      },
      Directive("text"),
      program.functions.map(x => genFunc(x, SymbolTable(None))),
      Label("main")
    )

    val mainSymTable: SymbolTable[Dest] = SymbolTable(None)
    val mainBody = lb(
      program.body.map(x => genStmt(x, mainSymTable))
    )

    instructions ++= lb(instructions, genFuncBody(List.empty, mainBody), Label("_exit"))

    val exitBody: ListBuffer[Instruction] = lb(
      // Align stack pointer to 16 bytes
      AndAsm(Immediate(-16), Rsp),
      CallAsm(Label("exit@plt"))
    )
    instructions ++= lb(genFuncBody(List.empty, exitBody))
    instructions ++= lb(
      genPrint(stringType, "%.*s"),
      genPrint(intType, "%d"),
      genPrint(printlnType, ""),
      genPrint(charType, "%c")
    )
    instructions.toList
  }

  private def genFunc(func: Func, symTable: SymbolTable[Dest]): ListBuffer[Instruction] =
    ListBuffer.empty // TODO

  private def genFuncBody(
      toSave: List[Reg],
      body: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    lb(
      saveRegs(toSave),
      body,
      restoreRegs(toSave),
      Ret
    )
  }

  // save the stack pointer to enter a new scope
  private def saveRegs(regs: List[Reg]): ListBuffer[Instruction] =
    lb(
      Push(Rbp),
      regs.map(r => Push(r)),
      Mov(Rbp, Rsp) // Set stack pointer to base pointer
    )

  // restore the stack pointer to exit a scope
  private def restoreRegs(regs: List[Reg]): ListBuffer[Instruction] = {
    lb(
      Mov(Rsp, Rbp),
      regs.map(r => Pop(r)),
      Pop(Rbp)
    )
  }

  private def genStmt(
      stmt: Stmt,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] =
    stmt match {
      case Skip       => lb()
      case Exit(expr) => genExit(expr, symTable)
      case Return(expr) =>
        lb(
          genExpr(expr, symTable),
          Ret
        )
      case _ => lb() // TODO
    }

  private def genExpr(
      expr: Expr,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = lb(
    expr match {
      case Integer(i) => Mov(Immediate(i.toLong), Eax())
    }
  )

  private def genExit(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] =
    lb(
      genExpr(expr, symTable),
      Mov(Edi(), Eax()),
      CallAsm(Label("_exit")),
      Mov(Immediate(0), Eax())
    )

  private lazy val stringType = 's'
  private lazy val intType = 'i'
  private lazy val charType = 'c'
  private lazy val printlnType = 'n'
  private def genPrint(typ: Char, format: String): ListBuffer[Instruction] = {
    val graph: ListBuffer[Instruction] = ListBuffer()
    graph += Directive("section .rodata")
    graph += Directive(s"int ${format.length}")
    graph += Label(s".print${typ}_format")
    graph += Directive(s"asciz \"$format\"")
    graph += Label(s"_print$typ")
    val printBody: ListBuffer[Instruction] = ListBuffer()
    printBody += AndAsm(Immediate(-16), Rsp)

    if (typ == stringType) {
      printBody += Mov(Edx(Size64), Edi(Size64))
      printBody += Mov(Esi(), Address(Immediate(-4), Edi(Size64)))
    } else if (typ == intType) {
      printBody += Mov(Esi(), Edi())
    } else if (typ == charType) {
      printBody += Mov(Esi(Size8), Edi(Size8))
    }

    printBody += Lea(Address(Label(s".print${typ}_format"), Rip), Edi(Size64))
    printBody += Mov(Immediate(0), Eax(Size8))

    if (typ == printlnType) {
      printBody += CallAsm(Label("puts@plt"))
    } else {
      printBody += CallAsm(Label("printf@plt"))
    }

    printBody += Mov(Immediate(0), Edi(Size64))
    printBody += CallAsm(Label("fflush@plt"))
    graph ++= genFuncBody(List.empty, printBody)
  }
}
