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
    var instructions = lb(
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

    instructions = lb(instructions, genFuncBody(List.empty, mainBody), Label("_exit"))

    val exitBody: ListBuffer[Instruction] = lb(
      // Align stack pointer to 16 bytes
      AndAsm(Rsp, Immediate(-16)),
      CallAsm(Label("exit@plt"))
    )

    lb(
      instructions,
      genFuncBody(List.empty, exitBody),
      stringLiters.keys.map(s => genPrintLiteral(s))
    ).toList

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
      case Skip       => ListBuffer()
      case Exit(expr) => genExit(expr, symTable)
      case Return(expr) =>
        ListBuffer().addAll(genExpr(expr, symTable)).addOne(Ret) // WHAT ##########################
      case _ => ListBuffer[Instruction]() // TODO
    }

  private def genExpr(
      expr: Expr,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = lb(
    expr match {
      case Integer(i) => Mov(Eax(), Immediate(i.toLong))
    }
  )

  private def genExit(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] =
    lb(
      genExpr(expr, symTable),
      Mov(Eax(), Edi()),
      CallAsm(Label("_exit")),
      Mov(Eax(), Immediate(0))
    )

  private def genPrintLiteral(s: String): ListBuffer[Instruction] = {
    val id: Int = stringLiters(s)
    val graph: ListBuffer[Instruction] = lb(Label(s"_print$id"))

    genFuncBody(
      List.empty,
      lb(
        AndAsm(Rsp, Immediate(-16)),
        Mov(Edi(), Edx()),
        Mov(Address(Immediate(-4), Edi()), Esi()),
        Lea(Edi(), Address(Label(s".L.str$id"), Esi())),
        Mov(Eax(), Immediate(0)),
        CallAsm(Label("printf@plt")),
        Mov(Edi(), Immediate(0)),
        CallAsm(Label("fflush@plt"))
      )
    )

  }
}
