package src.main.wacc

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object generator {

  def lb(instructions: Any*): ListBuffer[Instruction] = {
    val resultBuffer = ListBuffer[Instruction]()

    for (instruction <- instructions) {
      instruction match {
        case inst: Instruction =>
          resultBuffer += inst
        case listBuffer: ListBuffer[Instruction] => {
          resultBuffer ++= listBuffer
        }
        case list: List[Instruction] =>
          resultBuffer ++= list
        case _ =>
          throw new IllegalArgumentException(s"Unsupported type: ${instruction.getClass}")
      }
    }

    resultBuffer
  }

  val stringLiters: mutable.Map[String, Int] = mutable.Map.empty

  def generate(program: Program, formatter: Formatter): String = genProgram(program)
    .map(formatter(_))
    .mkString("\n")

  private def genProgram(program: Program): ListBuffer[Instruction] = {
    val instructions = lb(
      Directive("globl main"),
      genDataSection(stringLiters.view.mapValues(i => s".L.str$i").toSeq: _*),
      program.functions.map(x => genFunc(x, SymbolTable(None))),
      Label("main")
    )

    val mainSymTable: SymbolTable[Dest] = SymbolTable(None)
    val mainBody = lb(
      program.body.flatMap(x => genStmt(x, mainSymTable))
    )

    instructions ++= lb(genFuncBody(List.empty, mainBody), Label("_exit"))

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
      genPrint(charType, "%c"),
      genPrintBool,
      genRead(intType, "%d"),
      genRead(charType, "%c")
    )

    instructions
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
      case Skip         => lb()
      case Exit(expr)   => genExit(expr, symTable)
      case Return(expr) => lb(
        genExpr(expr, symTable),
        Ret
      )
      case Print(expr)   => genPrintStmt(symTable, expr)
      case PrintLn(expr) => genPrintStmt(symTable, expr) += CallAsm(Label("_println"))
      case Read(lval)    => genReadStmt(symTable, lval)
      case _            => lb() // TODO
    }

  private def genReadStmt(symTable: SymbolTable[Dest], lval: LVal): ListBuffer[Instruction] = {
    lb(
      genLVal(lval, symTable),
      Mov(Edi(Size64), Eax(Size64)),
      CallAsm(Label("_readi"))
    )
  }


  private def genPrintStmt(symTable: SymbolTable[Dest], expr: Expr): ListBuffer[Instruction] = {
    lb(
      genExpr(expr, symTable),
      Mov(Edi(), Eax()),
      expr.typ.get match {
        case CharType => CallAsm(Label("_printc"))
        case IntType => CallAsm(Label("_printi"))
        case StringType | ArrayType(CharType) => CallAsm(Label("_prints"))
        case BoolType => CallAsm(Label("_printb"))
        case _ => ???
      }
    )
  }

  private def genExit(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] =
    lb(
      genExpr(expr, symTable),
      Mov(Eax(), Edi()),
      CallAsm(Label("_exit")),
      Mov(Immediate(0), Eax())
    )

  private def genLVal(lval: LVal, symTable: SymbolTable[Dest]) = ???

  private def genExpr(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] = lb(
    expr match {
      case Integer(i) => Mov(Eax(), Immediate(i.toLong))
      case StringAtom(s) => Mov(Eax(Size64), Address(Rip, Label(s".L.str${stringLiters(s)}")))
    }
  )

  // Built-in functions

  private def genDataSection(data: (String, String)*): ListBuffer[Instruction] = lb(
    Directive("section .data"),
    lb(data.map(
      kv => lb(
        Directive(s"int ${kv._1.length}"),
        Label(kv._2),
        Directive(s"asciz \"${kv._1}\"")
      )
    ) : _*),
    Directive("text")
  )

  private lazy val stringType = 's'
  private lazy val intType = 'i'
  private lazy val charType = 'c'
  private lazy val printlnType = 'n'
  private def genPrint(typ: Char, format: String): ListBuffer[Instruction] = {
    val graph: ListBuffer[Instruction] = lb(
      genDataSection(format -> s".print${typ}_format"),
      Label(s"_print$typ")
    )
    val printBody: ListBuffer[Instruction] = lb(AndAsm(Rsp, Immediate(-16)))

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

  private val genPrintBool: ListBuffer[Instruction] = {
    val graph: ListBuffer[Instruction] = lb(
      genDataSection("true" -> ".printb_true_lit", "false" -> ".printb_false_lit"),
      Label("_printb")
    )
    val printBody: ListBuffer[Instruction] = lb(
      Cmp(Edi(Size8), Immediate(0)),
      Je(Label(".printb_true")),
      Lea(Edi(Size64), Address(Rip, Label(".printb_false_lit"))),
      Jmp(Label(".printb_end")),
      Label(".printb_true"),
      Lea(Edi(Size64), Address(Rip, Label(".printb_true_lit"))),
      Label(".printb_end"),
      CallAsm(Label("_prints")),
    )
    graph ++= genFuncBody(List.empty, printBody)
  }

  private def genRead(typ: Char, format: String): ListBuffer[Instruction] = {
    val instructions: ListBuffer[Instruction] = lb(
      Directive("section .rodata"),
      Directive(s"int ${format.length}"),
      Label(s".read${typ}_format"),
      Directive(s"asciz \"$format\""),
      Label(s"_read$typ")
    )
    val size = if (typ == intType) Size32 else Size8
    val readBody: ListBuffer[Instruction] = lb(
      AndAsm(Rsp, Immediate(-16)),
      SubAsm(Rsp, Immediate(16)),
      Mov(Edi(size), Address(Rsp)),
      Lea(Esi(Size64), Address(Rsp)),
      Lea(Edi(Size64), Address(Rip, Label(s".read${typ}_format"))),
      Mov(Eax(Size8), Immediate(0)),
      CallAsm(Label("scanf@plt")),
      Mov(Eax(size), Address(Rsp))
    )
    instructions ++= genFuncBody(List.empty, readBody)
  }
}
