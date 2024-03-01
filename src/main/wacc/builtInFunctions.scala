package src.main.wacc

import scala.collection.mutable.ListBuffer
import src.main.wacc.constants._
import src.main.wacc.Imm.intToImmediate

object builtInFunctions {

  private val maskRsp = AndAsm(-2 * ptrSize, Rsp)
  private val errOutOfMemory = "errOutOfMemory"
  private val errOutOfBounds = "errOutOfBounds"

  val dirGlobl: Directive = Directive("globl main")
  private val dirStr = "asciz"
  private val dirSectionData = Directive("section .data")

  val mainLabel: Label = Label("main")

  val errOverflow = "errOverflow"
  val errDivZero = "errDivZero"
  val errBadChar = "errBadChar"
  val errNull = "errNull"

  val exit = "exit"
  val free = "free"
  val freepair = "freepair"
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

  val boolTrueLit = "true"
  val boolFalseLit = "false"

  lazy val genBuiltInFunctions: ListBuffer[Instruction] = lb(
    genCall(exit, provided.exit),
    genPrint(stringType, "%.*s"),
    genPrint(intType, "%d"),
    genPrint(printlnType, ""),
    genPrint(charType, "%c"),
    genPrint(ptrType, "%p"),
    genPrintBool,
    genRead(intType, " %d"),
    genRead(charType, " %c"),
    genMalloc,
    genCall(free, provided.free),
    genFreePair,
    genArrAccess(Size8, store_? = true),
    genArrAccess(Size32, store_? = true),
    genArrAccess(Size64, store_? = true),
    genArrAccess(Size8, store_? = false),
    genArrAccess(Size32, store_? = false),
    genArrAccess(Size64, store_? = false),
    genErr(errOverflow, "fatal error: integer overflow or underflow occurred"),
    genErr(errDivZero, "fatal error: division or modulo by zero"),
    genErr(errOutOfMemory, "fatal error: out of memory"),
    genErr(errNull, "fatal error: null pair dereferenced or freed"),
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
    *    List of registers that need to be saved. The list should be in the same order as the
    *    variables are declared. In general, Allocator.(NON_)PARAM_REGS.take(n) should be used, where
    *    n is the number of variables declared in the scope.
    */
  def symTableEnterScope(
      symTable: SymbolTable[Dest],
      allocator: Allocator,
      toSave: List[Reg],
      mode: Mode = NonParamMode
  ): Unit = {
    // Offset addresses of variables by the amount that rbp has changed
    val offset = allocator.reservedSpace + // 1 or 2 for old rbp and return address
      (toSave.size + (if (mode == NonParamMode) 1 else 2)) * ptrSize

    // Update the symbol table to reflect the new addresses of the variables
    var st = Option(symTable)
    while (st.isDefined) {
      val table = st.get.table
      table.keySet.foreach(ident =>
        table.put(
          ident,
          table(ident) match {  // Update the address of the variable
            case Address(Rbp, Imm(pos), _, _) =>
              Address(Rbp, pos + offset)
            case r: Reg => r
            case _ =>
              throw new IllegalArgumentException("Variable addresses must be relative to Rbp")
          }
        )
      )
      st = st.get.parent
    }

    // Registers that were saved are now on stack
    toSave.reverse.zipWithIndex.foreach { case (r, i) =>
      val ident = symTable.reverseLookup(r).get
      symTable.put(ident, Address(Rbp, i * ptrSize))
    }
  }

  /** Updates the symbol table to reflect exit from a child scope
    *
    * @param symTable
    *   The symbol table to be updated
    * @param allocator
    *   The allocator of the parent scope
    * @param toSave
    *   List of registers that need to be saved. The list should be in the same order as the
    *   variables are declared. In general, Allocator.(NON_)PARAM_REGS.take(n) should be used, where
    *   n is the number of variables declared in the scope.
    */
  def symTableExitScope(
      symTable: SymbolTable[Dest],
      allocator: Allocator,
      toSave: List[Reg],
      mode: Mode = NonParamMode
  ): Unit = {
    // Offset addresses of variables by the amount that rbp has changed
    val offset = allocator.reservedSpace + // 1 or 2 for old rbp and return address
      (toSave.size + (if (mode == NonParamMode) 1 else 2)) * ptrSize

    // Registers that were saved on stack are back in their registers
    toSave.reverse.zipWithIndex.foreach { case (r, i) =>
      val ident =
        symTable.reverseLookup(Address(Rbp, i * ptrSize)).get // might be (i + 1)
      symTable.put(ident, r)
    }

    // Update the symbol table to reflect the new addresses of the variables
    var st = Option(symTable)
    while (st.isDefined) {
      val table = st.get.table
      table.keySet.foreach(ident =>
        table.put(
          ident,
          table(ident) match { // Update the address of the variable
            case Address(Rbp, Imm(pos), _, _) =>
              Address(Rbp, pos - offset)
            case r: Reg => r
            case _ =>
              throw new IllegalArgumentException("Variable addresses must be relative to Rbp")
          }
        )
      )
      st = st.get.parent
    }

  }

  /** Generates the assembly code for entering a new scope
   *
   * @param toSave The registers that should be saved
   * @param toAllocate The variables that need to be stack-allocated
   * @return The assembly code for entering a new scope
   */
  def genNewScopeEnter(
      toSave: List[Reg],
      toAllocate: List[SymbolTableObj]
  ): ListBuffer[Instruction] = {
    // Calculate the size of the stack frame
    val size = toAllocate.map(x => Allocator.getTypeWidth(x.typ.get)).sum
    lb(
      Push(Rbp), // Save the old base pointer
      toSave.map(r => Push(r)),
      Mov(Rsp, Rbp),
      SubAsm(size, Rsp)
    )
  }
  // Overload for when no new variables are in scope
  def genNewScopeEnter(): ListBuffer[Instruction] = {
    genNewScopeEnter(List.empty, List.empty)
  }
  // Overload to take list of variables to be saved
  def genNewScopeEnter(
      vars: List[SymbolTableObj]
  ): ListBuffer[Instruction] = {
    val toSave = Allocator.NON_PARAM_REGS.take(vars.size)
    val toAllocate = vars.drop(Allocator.NON_PARAM_REGS.size)
    genNewScopeEnter(toSave, toAllocate)
  }

  /** Generates the assembly code for exiting a scope
   *
   * @param toSave The registers that should be saved
   * @param toAllocate The variables that need to be stack-allocated
   * @return The assembly code for exiting a scope
   */
  def genNewScopeExit(
      toSave: List[Reg],
      toAllocate: List[SymbolTableObj]
  ): ListBuffer[Instruction] = {
    // Calculate the size of the stack frame
    val size = toAllocate.map(x => Allocator.getTypeWidth(x.typ.get)).sum
    lb(
      AddAsm(size, Rsp),
      Mov(Rbp, Rsp),
      toSave.reverse.map(r => { Pop(r) }),
      Pop(Rbp)
    )
  }
  // Overload for when no new variables are in scope
  def genNewScopeExit(): ListBuffer[Instruction] = {
    genNewScopeExit(List.empty, List.empty)
  }
  // Overload to take list of variables to be saved
  def genNewScopeExit(
      toAllocate: List[SymbolTableObj]
  ): ListBuffer[Instruction] = {
    val toSave = Allocator.NON_PARAM_REGS.take(toAllocate.size)
    genNewScopeExit(toSave, toAllocate)
  }

  // Generates assembly code for a string literal data section
  def genDataSection(data: (String, String)*): ListBuffer[Instruction] = lb(
    dirSectionData,
    lb(
      data.flatMap(kv =>
        lb( // String literals have their length, label, and string data
          Directive(s"int ${kv._1.length - kv._1.count(_ == '\\')}"),
          Label(kv._2),
          Directive(s"$dirStr \"${kv._1}\"")
        )
      )
    ),
    Directive("text")
  )

  // Generates assembly code for a simple external function call
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

  /** Generates assembly code for a print function
   *
   * @param typ The type of the print function:
   *            's' for string, 'i' for int, 'c' for char, 'n' for println, 'p' for pointer
   * @param format The format string for the print function
   * @return The assembly code for the print function
   */
  private def genPrint(typ: Char, format: String): ListBuffer[Instruction] = {
    val instructions: ListBuffer[Instruction] = lb(
      // Generate the format string for the print function
      genDataSection(format -> s".$print${typ}_format"),
      Label(s"_$print$typ")
    )
    val printBody: ListBuffer[Instruction] = lb(maskRsp)

    // Move the argument to the correct register
    if (typ == stringType) {
      printBody += Mov(Edi(Size64), Edx(Size64))
      // Move the length of the string to the correct register
      printBody += Mov(Address(Edi(Size64), -intSize), Esi())
    } else if (typ == intType) {
      printBody += Mov(Edi(), Esi())
    } else if (typ == charType) {
      printBody += Mov(Edi(Size8), Esi(Size8))
    } else if (typ == ptrType) {
      printBody += Mov(Edi(Size64), Esi(Size64))
    }

    // Load format string into register
    printBody += Lea(Address(Rip, Label(s".$print${typ}_format")), Edi(Size64))
    printBody += Mov(0, Eax(Size8))

    // Call the print function
    if (typ == printlnType) {
      printBody += CallAsm(provided.puts)
    } else {
      printBody += CallAsm(provided.printf)
    }

    // Flush the output buffer
    printBody += Mov(0, Edi(Size64))
    printBody += CallAsm(provided.fflush)
    instructions ++= lb(genNewScopeEnter(), printBody, genNewScopeExit(), Ret)
  }

  // Generates assembly code for a print function for booleans
  private val genPrintBool: ListBuffer[Instruction] = {
    // Generate string literals for true and false
    val graph: ListBuffer[Instruction] = lb(
      genDataSection(
        boolTrueLit -> s".$print${boolType}_${boolTrueLit}_lit",
        boolFalseLit -> s".$print${boolType}_${boolFalseLit}_lit"),
      Label(s"_$print$boolType")
    )
    // 1 represents true, 0 represents false
    val printBody: ListBuffer[Instruction] = lb(
      Cmp(boolTrue, Edi(Size8)), // Check if the boolean is true
      // If true, load the address of the true string literal into register
      JmpComparison(Label(s".$print${boolType}_$boolTrueLit"), Eq),
      // If false, load the address of the false string literal into register
      Lea(Address(Rip, Label(s".$print${boolType}_${boolFalseLit}_lit")), Edi(Size64)),
      Jmp(Label(s".$print${boolType}_end")),
      Label(s".$print${boolType}_$boolTrueLit"),
      Lea(Address(Rip, Label(s".$print${boolType}_${boolTrueLit}_lit")), Edi(Size64)),
      Label(s".$print${boolType}_end"),
      // Call the string print function
      CallAsm(Label(s"_$print$stringType")),
    )
    graph ++= lb(genNewScopeEnter(), printBody, genNewScopeExit(), Ret)
  }

  /** Generates assembly code for a read function
   *
   * @param typ The type of the read function:
   *            'i' for int, 'c' for char
   * @param format The format string for the read function
   * @return The assembly code for the read function
   */
  private def genRead(typ: Char, format: String): ListBuffer[Instruction] = {
    // Generate the format string for the read function
    val instructions: ListBuffer[Instruction] = lb(
      genDataSection(format -> s".read${typ}_format"),
      Label(s"_read$typ")
    )
    // Char is read as a byte, int is read as a 32-bit integer
    val size = if (typ == intType) Size32 else Size8
    val readBody: ListBuffer[Instruction] = lb(
      maskRsp,
      SubAsm(2 * ptrSize, Rsp),
      // Store default value
      Mov(Edi(size), Address(Rsp), useOpSize = true),
      Lea(Address(Rsp), Esi(Size64)),
      Lea(Address(Rip, Label(s".read${typ}_format")), Edi(Size64)),
      Mov(0, Eax(Size8)),
      // scanf will not overwrite the value if the input was EOF
      CallAsm(provided.scanf),
      // Move the read value to output register
      Movs(Address(Rsp), Eax(Size64), size, Size64),
      AddAsm(2 * ptrSize, Rsp)
    )
    instructions ++= lb(genNewScopeEnter(), readBody, genNewScopeExit(), Ret)
  }

  /** Generates assembly code for an error function
   *
   * @param name The name of the error function
   * @param msg The error message
   * @return The assembly code for the error function
   */
  private def genErr(name: String, msg: String): ListBuffer[Instruction] = {
    lb(
      genDataSection(s"$msg\\n" -> s".$name"),
      Label(s"_$name"),
      SubAsm(2 * ptrSize, Rsp),
      Lea(Address(Rip, Label(s".$name")), Edi(Size64)),
      CallAsm(Label("_prints")),
      // Exit with error code
      Mov(exitError, Edi(Size8)),
      CallAsm(provided.exit)
    )
  }

  /** Generates assembly code for an error function with one argument
   *
   *  Should be called with format string argument in %rsi
   *
   * @param name The name of the error function
   * @param msg The error message
   * @return The assembly code for the error function
   */
  private def genErr1Arg(name: String, msg: String): ListBuffer[Instruction] = {
    lb(
      genDataSection(s"$msg\\n" -> s".$name"),
      Label(s"_$name"),
      maskRsp,
      Lea(Address(Rip, Label(s".$name")), Edi(Size64)),
      Mov(0, Eax(Size8)),
      CallAsm(provided.printf),
      Mov(0, Edi(Size64)),
      CallAsm(provided.fflush),
      // Exit with error code
      Mov(exitError, Edi(Size8)),
      CallAsm(provided.exit)
    )
  }

  // Generates assembly code for the malloc function
  private val genMalloc: ListBuffer[Instruction] = lb(
    Label(s"_$malloc"),
    genNewScopeEnter(),
    lb(
      maskRsp,
      CallAsm(provided.malloc),
      Cmp(0, Eax(Size64)),
      JmpComparison(Label(s"_$errOutOfMemory"), Eq)
    ),
    genNewScopeExit(),
    Ret
  )

  /** Generates assembly code for array access
   *
   * Special calling convention:
   * R9: array address
   * R10: index
   * Rax: value to store (only or store)
   * Return: R9 = value (only for load)
   *
   * @param size The size of the array elements
   * @param store_? Whether the access is a store or a load
   * @return The assembly code for the array access
   */
  private def genArrAccess(size: Size, store_? : Boolean): ListBuffer[Instruction] = {
    val s = size match {
      case Size8  => byteSize
      case Size32 => intSize
      case Size64 => ptrSize
    }
    lb(
      Label(s"_arr${if (store_?) "Store" else "Load"}$s"),
      genNewScopeEnter(),
      lb(
        // Check if the index is negative
        Cmp(0, R10()),
        CMovl(R10(Size64), Esi(Size64)),
        JmpComparison(Label(s"_$errOutOfBounds"), Lt),
        // Check if the index is out of bounds
        // Array length is stored at the address of the array - intSize
        Mov(Address(R9(Size64), -intSize), Ebx()),
        Cmp(Ebx(), R10()),
        CMovge(R10(Size64), Esi(Size64)),
        JmpComparison(Label(s"_$errOutOfBounds"), GtEq),
        if (store_?)
          Mov(Eax(size),
            Address(R9(Size64), 0, R10(Size64), s), useOpSize = true)
        else
          Mov(Address(R9(Size64), 0, R10(Size64), s), R9(size))
      ),
      genNewScopeExit(),
      Ret
    )
  }

  // Generates assembly code for the free pair function
  private val genFreePair: ListBuffer[Instruction] = lb(
    Label(s"_$freepair"),
    genNewScopeEnter(),
    maskRsp,
    // Check if the pair isn't null
    Cmp(0, Edi(Size64)),
    JmpComparison(Label(s"_$errNull"), Eq),
    CallAsm(provided.free),
    genNewScopeExit(),
    Ret
  )

  def lb(instructions: Any*): ListBuffer[Instruction] = {
    val resultBuffer = ListBuffer[Instruction]()

    for (instruction <- instructions) {
      instruction match {
        case inst: Instruction => resultBuffer += inst
        case instIter: IterableOnce[Instruction] => resultBuffer ++= instIter
        case _ =>
          throw new IllegalArgumentException(s"Unsupported type: ${instruction.getClass}")
      }
    }

    resultBuffer
  }
}

// Provides labels for external functions
object provided {
  val exit: Label = Label("exit@plt")
  val puts: Label = Label("puts@plt")
  val printf: Label = Label("printf@plt")
  val fflush: Label = Label("fflush@plt")
  val scanf: Label = Label("scanf@plt")
  val malloc: Label = Label("malloc@plt")
  val free: Label = Label("free@plt")
}
