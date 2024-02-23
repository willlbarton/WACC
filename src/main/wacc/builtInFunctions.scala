package src.main.wacc

import scala.collection.mutable.ListBuffer
import src.main.wacc.generator.lb
import src.main.wacc.constants._

object builtInFunctions {

  private val maskRsp = AndAsm(Immediate(-16), Rsp)
  private val errOutOfMemory = "errOutOfMemory"
  private val errOutOfBounds = "errOutOfBounds"

  val dirGlobl: Directive = Directive("globl main")
  private val dirStr = "asciz"
  private val dirSectionData = Directive("section .data")

  val mainLabel: Label = Label("main")

  val errOverflow = "errOverflow"
  val errDivZero = "errDivZero"
  val errBadChar = "errBadChar"

  val exit = "exit"
  val free = "free"
  val malloc = "malloc"

  val print = "print"
  val read = "read"

  val stringType = 's'
  val intType = 'i'
  val charType = 'c'
  val printlnType = 'n'
  val ptrType = 'p'
  val boolType = 'b'

  val arrStore = "arrStore"
  val arrLoad = "arrLoad"

  lazy val genFunctions: ListBuffer[Instruction] = lb(
    genCall(exit, provided.exit),
    genPrint(stringType, "%.*s"),
    genPrint(intType, "%d"),
    genPrint(printlnType, ""),
    genPrint(charType, "%c"),
    genPrint(ptrType, "%p"),
    genPrintBool,
    genRead(intType, "%d"),
    genRead(charType, "%c"),
    genMalloc,
    genCall(free, provided.free),
    genArrAccess(Size8, direction = true),
    genArrAccess(Size32, direction = true),
    genArrAccess(Size64, direction = true),
    genArrAccess(Size8, direction = false),
    genArrAccess(Size32, direction = false),
    genArrAccess(Size64, direction = false),
    genErr(errOverflow, "fatal error: integer overflow or underflow occurred"),
    genErr(errDivZero, "fatal error: division or modulo by zero"),
    genErr(errOutOfMemory, "fatal error: out of memory"),
    genErr1Arg(errBadChar, "fatal error: int %d is not ascii character 0-127"),
    genErr1Arg(errOutOfBounds, "fatal error: array index %d out of bounds")
  )

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
    dirSectionData,
    lb(
      data.map(kv =>
        lb(
          Directive(s"int ${kv._1.length - kv._1.count(_ == '\\')}"),
          Label(kv._2),
          Directive(s"$dirStr \"${kv._1}\"")
        )
      ): _*
    ),
    Directive("text")
  )

  private def genCall(name: String, func: Label): ListBuffer[Instruction] = lb(
    Label(s"_$name"),
    genNewScope(
      lb(
        maskRsp,
        CallAsm(func)
      )
    ),
    Ret
  )

  private def genPrint(typ: Char, format: String): ListBuffer[Instruction] = {
    val graph: ListBuffer[Instruction] = lb(
      genDataSection(format -> s".print${typ}_format"),
      Label(s"_print$typ")
    )
    val printBody: ListBuffer[Instruction] = lb(maskRsp)

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

  private val genPrintBool: ListBuffer[Instruction] = {
    val graph: ListBuffer[Instruction] = lb(
      genDataSection("true" -> ".printb_true_lit", "false" -> ".printb_false_lit"),
      Label("_printb")
    )
    val printBody: ListBuffer[Instruction] = lb(
      Cmp(Immediate(1), Edi(Size8)),
      JmpComparison(Label(".printb_true"), Eq),
      Lea(Address(Rip, Label(".printb_false_lit")), Edi(Size64)),
      Jmp(Label(".printb_end")),
      Label(".printb_true"),
      Lea(Address(Rip, Label(".printb_true_lit")), Edi(Size64)),
      Label(".printb_end"),
      CallAsm(Label("_prints"))
    )
    graph ++= lb(genNewScope(printBody), Ret)
  }

  private def genRead(typ: Char, format: String): ListBuffer[Instruction] = {
    val instructions: ListBuffer[Instruction] = lb(
      genDataSection(format -> s".read${typ}_format"),
      Label(s"_read$typ")
    )
    val size = if (typ == intType) Size32 else Size8
    val readBody: ListBuffer[Instruction] = lb(
      maskRsp,
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

  private def genErr(name: String, msg: String): ListBuffer[Instruction] = {
    lb(
      genDataSection(s"$msg\\n" -> s".$name"),
      Label(s"_$name"),
      AddAsm(Immediate(-16), Rsp),
      Lea(Address(Rip, Label(s".$name")), Edi(Size64)),
      CallAsm(Label("_prints")),
      Mov(Immediate(-1), Eax(Size64)),
      CallAsm(provided.exit)
    )
  }

  // Should be called with format string argument in %rsi
  private def genErr1Arg(name: String, msg: String): ListBuffer[Instruction] = {
    lb(
      genDataSection(s"$msg\\n" -> s".$name"),
      Label(s"_$name"),
      AddAsm(Immediate(-16), Rsp),
      Lea(Address(Rip, Label(s".$name")), Edi(Size64)),
      Mov(Immediate(0), Eax(Size8)),
      CallAsm(provided.printf),
      Mov(Immediate(0), Edi(Size64)),
      CallAsm(provided.fflush),
      Mov(Immediate(-1), Edi(Size8)),
      CallAsm(provided.exit)
    )
  }

  private val genMalloc: ListBuffer[Instruction] = lb (
    Label(s"_$malloc"),
    genNewScope(lb(
      maskRsp,
      CallAsm(provided.malloc),
      Cmp(Immediate(0), Eax(Size64)),
      JmpComparison(Label(s"_$errOutOfMemory"), Eq),
    )),
    Ret
  )

  // Special calling convention:
  // R9: array address
  // R10: index
  // Rax: value to store (only or store)
  // Return: R9 = value (only for load)
  private def genArrAccess(size: Size, direction: Boolean): ListBuffer[Instruction] = {
    val s = size match {
      case Size8  => byteSize
      case Size32 => intSize
      case Size64 => ptrSize
    }
    lb(
      Label(s"_arr${if (direction) "Store" else "Load"}$s"),
      genNewScope(lb(
        Cmp(Immediate(0), R10()),
        CMovl(R10(Size64), Esi(Size64)),
        JmpComparison(Label(s"_$errOutOfBounds"), Lt),
        Mov(Address(R9(Size64), Immediate(-4)), Ebx()),
        Cmp(Ebx(), R10()),
        CMovge(R10(Size64), Esi(Size64)),
        JmpComparison(Label(s"_$errOutOfBounds"), GtEq),
        if (direction)
          Mov(Eax(Size64), Address(R9(Size64), Immediate(0), R10(Size64), Immediate(s)))
        else
          Mov(Address(R9(Size64), Immediate(0), R10(Size64), Immediate(s)), R9(Size64))
      )),
      Ret
    )
  }
}

object provided {
  val exit: Label = Label("exit@plt")
  val puts: Label = Label("puts@plt")
  val printf: Label = Label("printf@plt")
  val fflush: Label = Label("fflush@plt")
  val scanf: Label = Label("scanf@plt")
  val malloc: Label = Label("malloc@plt")
  val free: Label = Label("free@plt")
}
