package src.main.wacc

import scala.collection.mutable.ListBuffer
import src.main.wacc.generator.lb
import src.main.wacc.constants._

object builtInFunctions {

  private val maskRsp = AndAsm(Immediate(-16), Rsp)

  lazy val genFunctions: ListBuffer[Instruction] = lb(
    genCall("exit", provided.exit),
    genPrint(stringType, "%.*s"),
    genPrint(intType, "%d"),
    genPrint(printlnType, ""),
    genPrint(charType, "%c"),
    genPrint(ptrType, "%p"),
    genPrintBool,
    genRead(intType, "%d"),
    genRead(charType, "%c"),
    genMalloc,
    genCall("free", provided.free),
    genArrLoad(Size8),
    genArrLoad(Size32),
    genArrLoad(Size64),
    genErr("errOverflow", "fatal error: integer overflow or underflow occurred"),
    genErr("errDivZero", "fatal error: division or modulo by zero"),
    genErr("errOutOfMemory", "fatal error: out of memory"),
    genErr1Arg("errBadChar", "fatal error: int %d is not ascii character 0-127"),
    genErr1Arg("errOutOfBounds", "fatal error: array index %d out of bounds")
  )

  // // Main one
  // def genNewScope(
  //     body: ListBuffer[Instruction],
  //     toSave: List[Reg],
  //     toAllocate: List[SymbolTableObj],
  //     symTable: SymbolTable[Dest]
  // ): ListBuffer[Instruction] = {

  //   val size = toAllocate.map(x => Allocator.getTypeWidth(x.typ.get)).sum
  //   lb(
  //     Push(Rbp),
  //     toSave.zipWithIndex.map {
  //       case (r, i) => {
  //         var ident = symTable.reverseLookup(r).get

  //         Push(r)
  //       }
  //     },
  //     Mov(Rsp, Rbp),
  //     SubAsm(Immediate(size.toLong), Rsp),
  //     body,
  //     AddAsm(Immediate(size.toLong), Rsp),
  //     Mov(Rbp, Rsp),
  //     toSave.map(r => Pop(r)),
  //     Pop(Rbp)
  //   )
  // }
  // def genNewScope(
  //     body: ListBuffer[Instruction]
  // ): ListBuffer[Instruction] = {
  //   genNewScope(body, List.empty, List.empty, SymbolTable(None))
  // }
  // def genNewScope(
  //     body: ListBuffer[Instruction],
  //     vars: List[SymbolTableObj],
  //     symTable: SymbolTable[Dest]
  // ): ListBuffer[Instruction] = {
  //   val toSave = Allocator.NON_PARAM_REGS.take(vars.size)
  //   val toAllocate = vars.drop(Allocator.NON_PARAM_REGS.size)
  //   genNewScope(body, toSave, toAllocate, symTable)
  // }

  def genNewScopeEnter(
      toSave: List[Reg],
      toAllocate: List[SymbolTableObj],
      prevSize: Int,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = {
    val size = toAllocate.map(x => Allocator.getTypeWidth(x.typ.get)).sum
    lb(
      Push(Rbp),
      toSave.map(r => Push(r)), {
        toSave.reverse.zipWithIndex.map {
          case (r, i) => {
            // println(toSave)
            // println(r)
            // println(symTable.table)
            var ident = symTable.reverseLookup(r).get
            symTable.put(ident, Address(Rbp, Immediate(i.toLong * 8))) // might be (i + 1)
          }
        }

        var st = symTable.parent
        while (st.isDefined) {
          val table = st.get.table
          table.keySet.foreach(ident =>
            table.put(
              ident,
              table.get(ident).get match {
                case Address(Rbp, Immediate(offset), _, _) =>
                  Address(Rbp, Immediate(offset + prevSize))
                case _ => ???
              }
            )
          )
          st = st.get.parent
        }

        Mov(Rsp, Rbp)
      },
      SubAsm(Immediate(size.toLong), Rsp)
    )
  }

  def genNewScopeEnter(): ListBuffer[Instruction] = {
    genNewScopeEnter(List.empty, List.empty, 0, SymbolTable(None))
  }

  def genNewScopeEnter(
      toAllocate: List[SymbolTableObj],
      prevSize: Int,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = {
    val toSave = Allocator.NON_PARAM_REGS.take(toAllocate.size)
    genNewScopeEnter(toSave, toAllocate, prevSize, symTable)
  }

  def genNewScopeExit(
      toSave: List[Reg],
      toAllocate: List[SymbolTableObj],
      prevSize: Int,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = {
    val size = toAllocate.map(x => Allocator.getTypeWidth(x.typ.get)).sum

    lb(
      AddAsm(Immediate(size.toLong), Rsp),
      Mov(Rbp, Rsp),
      toSave.map(r => { Pop(r) }), {

        toSave.zipWithIndex.map {
          case (r, i) => {
            var ident =
              symTable.reverseLookup(Address(Rbp, Immediate(i.toLong * 8))).get // might be (i + 1)
            symTable.put(ident, r)
          }
        }

        var st = symTable.parent
        while (st.isDefined) {
          val table = st.get.table
          table.keySet.foreach(ident =>
            table.put(
              ident,
              table.get(ident).get match {
                case Address(Rbp, Immediate(offset), _, _) =>
                  Address(Rbp, Immediate(offset - (prevSize + 8)))
                case _ => ???
              }
            )
          )
          st = st.get.parent
        }
        Pop(Rbp)
      }
    )
  }

  def genNewScopeExit(): ListBuffer[Instruction] = {
    genNewScopeExit(List.empty, List.empty, 0, SymbolTable(None))
  }

  def genNewScopeExit(
      toAllocate: List[SymbolTableObj],
      prevSize: Int,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = {
    val toSave = Allocator.NON_PARAM_REGS.take(toAllocate.size)
    genNewScopeExit(toSave, toAllocate, prevSize, symTable)
  }

  def genDataSection(data: (String, String)*): ListBuffer[Instruction] = lb(
    Directive("section .data"),
    lb(
      data.map(kv =>
        lb(
          Directive(s"int ${kv._1.length - kv._1.count(_ == '\\')}"),
          Label(kv._2),
          Directive(s"asciz \"${kv._1}\"")
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
    // genNewScope(
    //   lb(
    //     maskRsp,
    //     CallAsm(func)
    //   )
    // ),
    Ret
  )

  private lazy val stringType = 's'
  private lazy val intType = 'i'
  private lazy val charType = 'c'
  private lazy val printlnType = 'n'
  private lazy val ptrType = 'p'
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
    Label("_malloc"),
    genNewScopeEnter(),
    (
      lb(
        maskRsp,
        CallAsm(provided.malloc),
        Cmp(Immediate(0), Eax(Size64)),
        JmpComparison(Label("_errOutOfMemory"), Eq)
      )
    ),
    genNewScopeExit(),
    Ret
  )

  // Special calling convention:
  // R9: array address
  // R10: index
  // Return: R9 = array[index]
  private def genArrLoad(size: Size): ListBuffer[Instruction] = {
    val s = size match {
      case Size8  => byteSize
      case Size32 => intSize
      case Size64 => ptrSize
    }
    lb(
      Label(s"_arrLoad$s"),
      genNewScopeEnter(),
      lb(
        Cmp(Immediate(0), R10()),
        CMovl(R10(Size64), Esi(Size64)),
        JmpComparison(Label("_errOutOfBounds"), Lt),
        Mov(Address(R9(Size64), Immediate(-4)), Ebx()),
        Cmp(Ebx(), R10()),
        CMovge(R10(Size64), Esi(Size64)),
        JmpComparison(Label("_errOutOfBounds"), Eq),
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
