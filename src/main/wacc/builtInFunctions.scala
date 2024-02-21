package src.main.wacc

import scala.collection.mutable.ListBuffer
import src.main.wacc.generator.lb

object builtInFunctions {

  def genNewScope(
    body: ListBuffer[Instruction],
    toSave: List[Reg],
    toAllocate: List[SymbolTableObj]
  ): ListBuffer[Instruction] = {
    val size = toAllocate.map(x => Allocator.getTypeWidth(x.typ.get)).sum
    lb(
      Push(Rbp),
      toSave.map(r => Push(r)),
      Mov(Rsp, Rbp),
      SubAsm(Immediate(size.toLong), Rsp),
      body,
      AddAsm(Immediate(size.toLong), Rsp),
      Mov(Rbp, Rsp),
      toSave.map(r => Pop(r)),
      Pop(Rbp)
    )
  }
  def genNewScope(body: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    genNewScope(body, List.empty, List.empty)
  }
  def genNewScope(
                           body: ListBuffer[Instruction],
                           vars: List[SymbolTableObj]
                         ): ListBuffer[Instruction] = {
    val toSave = Allocator.NON_PARAM_REGS.take(vars.size)
    val toAllocate = vars.drop(Allocator.NON_PARAM_REGS.size)
    genNewScope(body, toSave, toAllocate)
  }

  def genDataSection(data: (String, String)*): ListBuffer[Instruction] = lb(
    Directive("section .data"),
    lb(
      data.map(kv =>
        lb(
          Directive(s"int ${kv._1.length}"),
          Label(kv._2),
          Directive(s"asciz \"${kv._1}\"")
        )
      ): _*
    ),
    Directive("text")
  )

  lazy val stringType = 's'
  lazy val intType = 'i'
  lazy val charType = 'c'
  lazy val printlnType = 'n'
  lazy val ptrType = 'p'
  def genPrint(typ: Char, format: String): ListBuffer[Instruction] = {
    val graph: ListBuffer[Instruction] = lb(
      genDataSection(format -> s".print${typ}_format"),
      Label(s"_print$typ")
    )
    val printBody: ListBuffer[Instruction] = lb(AndAsm(Immediate(-16), Rsp))

    if (typ == stringType) {
      printBody += Mov(Edi(Size64), Edx(Size64))
      printBody += Mov(Address(Edi(Size64), Immediate(-4)), Esi())
    } else if (typ == intType) {
      printBody += Mov(Edi(), Esi())
    } else if (typ == charType) {
      printBody += Mov(Edi(Size8), Esi(Size8))
    } else if (typ == ptrType) {
      printBody += Mov(Edi(Size64), Esi(Size64))
    }

    printBody += Lea(Address(Rip, Label(s".print${typ}_format")), Edi(Size64))
    printBody += Mov(Immediate(0), Eax(Size8))

    if (typ == printlnType) {
      printBody += CallAsm(provided.puts)
    } else {
      printBody += CallAsm(provided.printf)
    }

    printBody += Mov(Immediate(0), Edi(Size64))
    printBody += CallAsm(provided.fflush)
    graph ++= lb(genNewScope(printBody), Ret)
  }

  val genPrintBool: ListBuffer[Instruction] = {
    val graph: ListBuffer[Instruction] = lb(
      genDataSection("true" -> ".printb_true_lit", "false" -> ".printb_false_lit"),
      Label("_printb")
    )
    val printBody: ListBuffer[Instruction] = lb(
      Cmp(Immediate(1), Edi(Size8)),
      Je(Label(".printb_true")),
      Lea(Address(Rip, Label(".printb_false_lit")), Edi(Size64)),
      Jmp(Label(".printb_end")),
      Label(".printb_true"),
      Lea(Address(Rip, Label(".printb_true_lit")), Edi(Size64)),
      Label(".printb_end"),
      CallAsm(Label("_prints"))
    )
    graph ++= lb(genNewScope(printBody), Ret)
  }

  def genRead(typ: Char, format: String): ListBuffer[Instruction] = {
    val instructions: ListBuffer[Instruction] = lb(
      genDataSection(format -> s".read${typ}_format"),
      Label(s"_read$typ")
    )
    val size = if (typ == intType) Size32 else Size8
    val readBody: ListBuffer[Instruction] = lb(
      AndAsm(Immediate(-16), Rsp),
      SubAsm(Immediate(16), Rsp),
      Mov(Address(Rsp), Edi(size)),
      Lea(Address(Rsp), Esi(Size64)),
      Lea(Address(Rip, Label(s".read${typ}_format")), Edi(Size64)),
      Mov(Immediate(0), Eax(Size8)),
      CallAsm(provided.scanf),
      Mov(Address(Rsp), Eax(size))
    )
    instructions ++= lb(genNewScope(readBody), Ret)
  }

  def genErr(name: String, msg: String): ListBuffer[Instruction] = {
    lb(
      genDataSection(s"$msg\\n" -> s".$name"),
      Label(s"_$name"),
      AddAsm(Immediate(-16), Rsp),
      Lea(Address(Rip, Label(s".$name")), Edi(Size64)),
      CallAsm(Label("_prints")),
      Mov(Immediate(-1), Eax(Size64)),
      CallAsm(Label("_exit"))
    )
  }
}

object provided {
  val puts: Label = Label("puts@plt")
  val printf: Label = Label("printf@plt")
  val fflush: Label = Label("fflush@plt")
  val scanf: Label = Label("scanf@plt")
  val malloc: Label = Label("malloc@plt")
}
