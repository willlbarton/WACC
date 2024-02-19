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
      AndAsm(Rsp, Immediate(-16)),
      CallAsm(Label("exit@plt"))
    )
    instructions ++= lb(genFuncBody(List.empty, exitBody))
    instructions ++= lb(
      genPrint(stringType, "%.*s"),
      genPrint(intType, "%d"),
      genPrint(printlnType, ""),
      genPrint(charType, "%c"),
      genPrintBool
    )
    instructions.toList
  }

  private def genFunc(func: Func, symTable: SymbolTable[Dest]): ListBuffer[Instruction] = ListBuffer.empty // TODO

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
      Mov(Rsp, Rbp) // Set stack pointer to base pointer
    )

  // restore the stack pointer to exit a scope
  private def restoreRegs(regs: List[Reg]): ListBuffer[Instruction] = {
    lb(
      Mov(Rbp, Rsp),
      regs.map(r => Pop(r)),
      Pop(Rbp)
    )
  }

  private def genStmt(
      stmt: Stmt,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] =
    stmt match {
      case Skip         => lb()
      case Exit(expr)   => genExit(expr, symTable)
      case Return(expr) => lb(
        genExpr(expr, symTable),
        Ret
      )
      case Print(expr)   => genPrintStmt(symTable, expr)
      case PrintLn(expr) => genPrintStmt(symTable, expr) += CallAsm(Label("_println"))
      case _            => lb() // TODO
    }

  private def genPrintStmt(symTable: SymbolTable[Dest], expr: Expr): ListBuffer[Instruction] = {
    lb(
      genExpr(expr, symTable),
      Mov(Edi(), Eax()),
      expr.typ.get match {
        case CharType => CallAsm(Label("_printc"))
        case IntType => CallAsm(Label("_printi"))
        case StringType => CallAsm(Label("_prints"))
        case BoolType => CallAsm(Label("_printbool"))
        case _ => ???
      }
    )
  }

  private def genExit(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] =
    lb(
      genExpr(expr, symTable),
      Mov(Eax(), Edi()),
      CallAsm(Label("_exit")),
      Mov(Eax(), Immediate(0))
    )

  private def genExpr(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] = lb(
    expr match {
      case Integer(i) => Mov(Eax(), Immediate(i.toLong))
    }
  )

  private lazy val stringType = 's'
  private lazy val intType = 'i'
  private lazy val charType = 'c'
  private lazy val printlnType = 'n'
  private def genPrint(typ: Char, format: String): ListBuffer[Instruction] = {
    val graph: ListBuffer[Instruction] = lb(
       Directive("section .rodata"),
       Directive(s"int ${format.length}"),
       Label(s".print${typ}_format"),
       Directive(s"asciz \"$format\""),
       Label(s"_print$typ")
    )
    val printBody: ListBuffer[Instruction] = lb(AndAsm(Rsp, Immediate(-16)))

    if (typ == stringType) {
      printBody += Mov(Edi(Size64), Edx(Size64))
      printBody += Mov(Address(Immediate(-4), Edi(Size64)), Esi())
    } else if (typ == intType) {
      printBody += Mov(Edi(), Esi())
    } else if (typ == charType) {
      printBody += Mov(Edi(Size8), Esi(Size8))
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

  private val genPrintBool: ListBuffer[Instruction] = {
    val graph: ListBuffer[Instruction] = lb(
      Directive("section .rodata"),
      Directive("int 4"),
      Label(".printb_true_lit"),
      Directive("asciz \"true\""),
      Directive("int 5"),
      Label(".printb_false_lit"),
      Directive("asciz \"false\""),
      Label("_printbool")
    )
    val printBody: ListBuffer[Instruction] = lb(
      Cmp(Edi(Size8), Immediate(0)),
      Je(Label(".printb_true")),
      Lea(Edi(Size64), Address(Label(".printb_false_lit"), Rip)),
      Jmp(Label(".printb_end")),
      Label(".printb_true"),
      Lea(Edi(Size64), Address(Label(".printb_true_lit"), Rip)),
      Label(".printb_end"),
      CallAsm(Label("_prints")),
    )
    graph ++= genFuncBody(List.empty, printBody)
  }
}
