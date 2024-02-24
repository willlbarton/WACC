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

  /** Updates the symbol table to reflect entry to a new child scope
    *
    * @param symTable
    *   The symbol table to be updated
    * @param allocator
    *   The allocator of the parent scope
    * @param toSave
    *   List of registers that need to be saved. It is always safe to use Allocator.NON_PARAM_REGS,
    *   but not all of them are always needed. The list should be in the same order as the variables
    *   are declared. In general, Allocator.NON_PARAM_REGS.take(n) should be used, where n is the
    *   number of variables declared in the scope.
    */
  def symTableEnterScope(
      symTable: SymbolTable[Dest],
      allocator: Allocator,
      toSave: List[Reg]
  ): Unit = {

    var st = Option(symTable)
    while (st.isDefined) {
      val table = st.get.table
      table.keySet.foreach(ident =>
        table.put(
          ident,
          table(ident) match {
            case Address(Rbp, Immediate(offset), _, _) =>
              Address(Rbp, Immediate(offset + allocator.reservedSpace + ptrSize + toSave.size * 8))
            case r: Reg => r
            case _ =>
              throw new IllegalArgumentException("Variable addresses must be relative to Rbp")
          }
        )
      )
      st = st.get.parent
    }

    toSave.reverse.zipWithIndex.foreach { case (r, i) =>
      val ident = symTable.reverseLookup(r).get
      symTable.put(ident, Address(Rbp, Immediate(i * 8))) // might be (i + 1)
    }
  }

  /** Updates the symbol table to reflect exit from a child scope
    *
    * @param symTable
    *   The symbol table to be updated
    * @param allocator
    *   The allocator of the parent scope
    * @param toSave
    *   List of registers that need to be saved. It is always safe to use Allocator.NON_PARAM_REGS,
    *   but not all of them are always needed. The list should be in the same order as the variables
    *   are declared. In general, Allocator.NON_PARAM_REGS.take(n) should be used, where n is the
    *   number of variables declared in the scope.
    */
  def symTableExitScope(
      symTable: SymbolTable[Dest],
      allocator: Allocator,
      toSave: List[Reg]
  ): Unit = {

    toSave.reverse.zipWithIndex.foreach { case (r, i) =>
      val ident =
        symTable.reverseLookup(Address(Rbp, Immediate(i * 8))).get // might be (i + 1)
      symTable.put(ident, r)
    }

    var st = Option(symTable)
    while (st.isDefined) {
      val table = st.get.table
      table.keySet.foreach(ident =>
        table.put(
          ident,
          table(ident) match {
            case Address(Rbp, Immediate(offset), _, _) =>
              Address(
                Rbp,
                Immediate(offset - allocator.reservedSpace - (toSave.size + 1) * ptrSize)
              )
            case r: Reg => r
            case _ =>
              throw new IllegalArgumentException("Variable addresses must be relative to Rbp")
          }
        )
      )
      st = st.get.parent
    }

  }

  def genNewScopeEnter(
      toSave: List[Reg],
      toAllocate: List[SymbolTableObj]
  ): ListBuffer[Instruction] = {
    val size = toAllocate.map(x => Allocator.getTypeWidth(x.typ.get)).sum
    lb(
      Push(Rbp),
      toSave.map(r => Push(r)),
      Mov(Rsp, Rbp),
      SubAsm(Immediate(size), Rsp)
    )
  }
  def genNewScopeEnter(): ListBuffer[Instruction] = {
    genNewScopeEnter(List.empty, List.empty)
  }
  def genNewScopeEnter(
      vars: List[SymbolTableObj]
  ): ListBuffer[Instruction] = {
    val toSave = Allocator.NON_PARAM_REGS.take(vars.size)
    val toAllocate = vars.drop(Allocator.NON_PARAM_REGS.size)
    genNewScopeEnter(toSave, toAllocate)
  }

  def genNewScopeExit(
      toSave: List[Reg],
      toAllocate: List[SymbolTableObj]
  ): ListBuffer[Instruction] = {
    val size = toAllocate.map(x => Allocator.getTypeWidth(x.typ.get)).sum

    lb(
      AddAsm(Immediate(size), Rsp),
      Mov(Rbp, Rsp),
      toSave.reverse.map(r => { Pop(r) }),
      Pop(Rbp)
    )
  }
  def genNewScopeExit(): ListBuffer[Instruction] = {
    genNewScopeExit(List.empty, List.empty)
  }
  def genNewScopeExit(
      toAllocate: List[SymbolTableObj]
  ): ListBuffer[Instruction] = {
    val toSave = Allocator.NON_PARAM_REGS.take(toAllocate.size)
    genNewScopeExit(toSave, toAllocate)
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
    genNewScopeEnter(),
    lb(
      maskRsp,
      CallAsm(func)
    ),
    genNewScopeExit(),
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
    graph ++= lb(genNewScopeEnter(), printBody, genNewScopeExit(), Ret)
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
    graph ++= lb(genNewScopeEnter(), printBody, genNewScopeExit(), Ret)
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
    instructions ++= lb(genNewScopeEnter(), readBody, genNewScopeExit(), Ret)
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

  private val genMalloc: ListBuffer[Instruction] = lb(
    Label(s"_$malloc"),
    genNewScopeEnter(),
    lb(
      maskRsp,
      CallAsm(provided.malloc),
      Cmp(Immediate(0), Eax(Size64)),
      JmpComparison(Label(s"_$errOutOfMemory"), Eq)
    ),
    genNewScopeExit(),
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
      genNewScopeEnter(),
      lb(
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
      ),
      genNewScopeExit(),
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
