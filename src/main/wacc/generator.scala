package src.main.wacc

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import src.main.wacc.constants._
import src.main.wacc.builtInFunctions._
import builtInFunctions.lb
import src.main.wacc.Imm.intToImmediate

object generator {

  // string literals to be stored in the data section
  val stringLiters: mutable.Map[String, Int] = mutable.Map.empty

  def generate(program: Program): (ListBuffer[Instruction], Map[Ident, ListBuffer[Instruction]]) = {
    val instructions = lb(
      dirGlobl,
      // Data section
      genDataSection(stringLiters.view.mapValues(i => s".L.str$i").toSeq: _*),
      mainLabel
    )

    val mainSymTable: SymbolTable[Dest] = SymbolTable(None)
    val allocator = Allocator(program.vars)

    // Generate the main body
    val mainBody = lb(
      genStmts(program.body, mainSymTable, allocator),
      Mov(exitSuccess, Eax(Size64))
    )


    // Main body should be evaluated before symTableEnterScope as the symbol table needs to be
    // updated with the variables in the main body
    val savedRegs = Allocator.NON_PARAM_REGS.take(program.vars.size)
    symTableEnterScope(mainSymTable, allocator, savedRegs)

    val functions = program.functions.map(x => x.ident -> genFunc(x))

    instructions ++= lb(
      genNewScopeEnter(program.vars),
      mainBody,
      genNewScopeExit(program.vars),
      Ret,
      // Generate the functions
      functions.flatMap(_._2),
      // Generate the built-in functions
      genBuiltInFunctions
    )
    symTableExitScope(mainSymTable, allocator, savedRegs)
    (instructions, functions.map { case (k,v) => k -> v.tail.init }.toMap)
  }

  // Generates the assembly for a single user defined function
  private def genFunc(
      func: Func
  ): ListBuffer[Instruction] = {
    // Allocate space for the parameters
    val allocator = Allocator(
      func.params.drop(Allocator.PARAM_REGS.length).map(x => Allocator.getTypeWidth(x.t)).sum,
      ParamMode
    )

    // Populate the symbol table with the parameters
    val paramTable = SymbolTable[Dest](None)
    func.params.foreach { param =>
      paramTable.put(param.ident, allocator.allocateSpace(param.t))
    }

    val usedParamRegs = Allocator.PARAM_REGS.take(func.params.length)
    // New scope is created for the parameters
    symTableEnterScope(paramTable, allocator, usedParamRegs, ParamMode)

    val exitScope = genNewScopeExit(usedParamRegs, func.vars)

    val toAllocate = func.vars.drop(Allocator.NON_PARAM_REGS.length)

    val instructions = lb(
      Label(s"wacc_${func.ident.name}"),
      genNewScopeEnter(usedParamRegs, toAllocate),
      // Generate the body of the function
      genStmts(func.body, paramTable.makeChild, Allocator(toAllocate, NonParamMode), exitScope)
    )

    symTableExitScope(paramTable, allocator, usedParamRegs, ParamMode)

    instructions
  }

  // Generates the assembly for a list of statements
  private def genStmts(
      stmts: ListBuffer[Stmt],
      symTable: SymbolTable[Dest],
      allocator: Allocator,
      exitScope: ListBuffer[Instruction]
  ): ListBuffer[Instruction] =
    lb(
      stmts.flatMap(x => {
        genStmt(x, symTable, allocator, exitScope)
      })
    )

  private def genStmts(
      stmts: List[Stmt],
      symTable: SymbolTable[Dest],
      allocator: Allocator,
      exitScope: ListBuffer[Instruction] = lb()
  ): ListBuffer[Instruction] =
    genStmts(
      ListBuffer() ++= stmts,
      symTable,
      allocator,
      exitScope
    )

  // Generates the assembly for a single statement
  private def genStmt(
      stmt: Stmt,
      symTable: SymbolTable[Dest],
      allocator: Allocator,
      exitScope: ListBuffer[Instruction] = lb()
  ): ListBuffer[Instruction] =
    stmt match {
      case Skip       => lb()
      case Exit(expr) => genExit(expr, symTable)
      case Return(expr) => lb(
        genExpr(expr, symTable),
        Pop(Eax(Size64)),
        exitScope,
        Ret
      )
      case Print(expr)   => genPrintStmt(symTable, expr)
      case PrintLn(expr) => genPrintStmt(symTable, expr) += CallAsm(Label(s"_$print$printlnType"))
      case Read(lval)    => genReadStmt(symTable, lval)
      case Decl(t, ident, value) =>
        val dest = allocator.allocateSpace(t)
        symTable.put(ident, dest) // Add the variable to the symbol table
        genDeclStmt(value, dest, symTable, allocator)
      case Asgn(lval, value) => genAsgnStmt(lval, value, symTable, allocator)
      case Free(expr)        => genFreeStmt(expr, symTable)
      case ifStmt: IfStmt    => genIfStmt(ifStmt, symTable, allocator, exitScope)
      case s @ ScopedStmt(stmts) =>
        genScopedStmt(stmts, s.vars, symTable, allocator, NonParamMode, lb(), exitScope)
      case w @ While(expr, stmts) => genWhile(expr, stmts, w.vars, symTable, allocator, exitScope)
    }

  // Generates the assembly for a list of statements in a new scope
  private def genScopedStmt(
      stmts: List[Stmt],
      vars: List[SymbolTableObj],
      symTable: SymbolTable[Dest],
      allocator: Allocator,
      mode: Mode = NonParamMode,
      extraInstructions: ListBuffer[Instruction] = lb(),
      exitScope: ListBuffer[Instruction] = lb()
  ): ListBuffer[Instruction] = {

    val used = allocator.usedRegs

    symTableEnterScope(symTable, allocator, used)

    val toAllocate = vars.drop(
      (if (mode == ParamMode) Allocator.PARAM_REGS else Allocator.NON_PARAM_REGS).size
    )

    val exitScope2 = genNewScopeExit(used, toAllocate)

    val instructions = lb(
      genNewScopeEnter(used, toAllocate),
      genStmts(stmts, symTable.makeChild, Allocator(vars, mode), exitScope2 ++ exitScope),
      extraInstructions,
      exitScope2
    )

    symTableExitScope(symTable, allocator, used)
    instructions
  }

  // Generates the assembly for a loop
  private def genWhile(
      expr: Expr,
      stmts: List[Stmt],
      vars: List[SymbolTableObj],
      symTable: SymbolTable[Dest],
      allocator: Allocator,
      exitScope: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    val labelExpr = Allocator.allocateLabel
    val labelStmts = Allocator.allocateLabel

    val generatedExpr = genExpr(expr, symTable)

    lb(
      Jmp(labelExpr), // Evaluate condition
      labelStmts,
      // Generate the body of the loop in a new scope
      genScopedStmt(stmts, vars, symTable, allocator, NonParamMode, lb(), exitScope),
      labelExpr,
      generatedExpr,
      Pop(Eax(Size64)),
      Cmp(boolTrue, Eax(Size64)),
      JmpComparison(labelStmts, Eq)
    )
  }

  // Generates the assembly for an if statement
  private def genIfStmt(
      ifStmt: IfStmt,
      symTable: SymbolTable[Dest],
      allocator: Allocator,
      exitScope: ListBuffer[Instruction] = lb()
  ): ListBuffer[Instruction] = {
    val IfStmt(expr, body1, body2) = ifStmt
    val labelTrue = Allocator.allocateLabel
    val labelContinue = Allocator.allocateLabel
    lb(
      // Check the condition
      genExpr(expr, symTable),
      Pop(Eax(Size64)),
      Movs(Eax(Size8), Eax(Size64), Size8, Size64),
      Cmp(boolTrue, Eax(Size64)),
      // If the condition is true, execute the first branch
      JmpComparison(labelTrue, Eq),
      genScopedStmt(body2, ifStmt.branch2Vars, symTable, allocator, NonParamMode, lb(), exitScope),
      // Skip to end after executing the 'false' branch
      Jmp(labelContinue),
      labelTrue,
      genScopedStmt(body1, ifStmt.branch1Vars, symTable, allocator, NonParamMode, lb(), exitScope),
      labelContinue
    )
  }

  // Generates the assembly for a declaration statement
  private def genDeclStmt(
      value: RVal,
      dest: Dest,
      symTable: SymbolTable[Dest],
      allocator: Allocator
  ): ListBuffer[Instruction] = lb(
    genRval(value, symTable, allocator),
    Pop(Eax(Size64)),
    Mov(
      // If the destination is a register, use the register size, otherwise use the size of the type
      Eax(if (dest.isInstanceOf[Reg]) Size64 else Allocator.getTypeSize(value.typ.get)),
      dest,
      useOpSize = true
    )
  )

  // Generates the assembly for an assignment statement
  private def genAsgnStmt(
      lval: LVal,
      value: RVal,
      symTable: SymbolTable[Dest],
      allocator: Allocator
  ): ListBuffer[Instruction] = {
    lval match {
      case id: Ident => lb(
        genRval(value, symTable, allocator),
        Pop(Eax(Size64)),
        Mov(
          // If the destination is a register, use the register size,
          // otherwise use the size of the type
          Eax(if (symTable(id).get.isInstanceOf[Reg]) Size64
            else Allocator.getTypeSize(value.typ.get)),
          symTable(id).get,
          useOpSize = true
        )
      )
      case arr @ ArrayElem(ident, exprs) =>
        var typ = ident.typ.get
        // Get the type of the array, fold to get nested array type
        typ = exprs.foldLeft(typ) { case (t, _) =>
          t match {
            case ArrayType(t) => t
            case _            => throw new IllegalArgumentException(s"Type $t was not an array")
          }
        }
        lb(
          genLVal(arr, symTable),
          genExpr(exprs.last, symTable),
          genRval(value, symTable, allocator),
          Pop(Eax(Size64)),
          Pop(R10(Size64)),
          Pop(R9(Size64)),
          // Store using the arrStore function
          CallAsm(Label(s"_$arrStore${Allocator.getTypeWidth(typ)}"))
        )
      case f @ Fst(_) => asgnPairElem(f, value, symTable, allocator)
      case s @ Snd(_) => asgnPairElem(s, value, symTable, allocator, snd_? = true)
    }
  }

  // Generates the assembly for a pair element assignment
  private def asgnPairElem(
      left: LVal,
      right: RVal,
      symTable: SymbolTable[Dest],
      allocator: Allocator,
      snd_? : Boolean = false
  ): ListBuffer[Instruction] = lb(
    genLVal(left, symTable),
    genRval(right, symTable, allocator),
    Pop(Eax(Size64)),
    Pop(Ebx(Size64)),
    // null checks are done already
    Mov(Eax(Size64), Address(Ebx(Size64)))
  )

  // Generates the assembly for a free statement
  private def genFreeStmt(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] =
    expr.typ.get match {
      case ArrayType(_) => lb(
        genExpr(expr, symTable),
        Pop(Eax(Size64)),
        Mov(Eax(Size64), Edi(Size64)),
        // The length of the array is stored at the address before the start of the array
        SubAsm(intSize, Edi(Size64)),
        CallAsm(Label(s"_$free"))
      )
      // call freePair for pairs
      case PairType(_, _) | Pair => lb(
        genExpr(expr, symTable),
        Pop(Eax(Size64)),
        Mov(Eax(Size64), Edi(Size64)),
        CallAsm(Label(s"_$freepair"))
      )
    }

  // Generates the assembly for an RVal of an assignment or declaration
  private def genRval(
      value: RVal,
      symTable: SymbolTable[Dest],
      allocator: Allocator
  ): ListBuffer[Instruction] =
    value match {
      case e: Expr       => genExpr(e, symTable)
      case a: ArrayLiter => genArray(a, symTable)
      case c: Call       => genCall(c, symTable, allocator)
      case NewPair(a, b) => genPair(a, b, symTable)
      // Always dereference pairs elements at this stage
      case f @ Fst(_)    => genPairElem(f, symTable, deref_? = true)
      case s @ Snd(_)    => genPairElem(s, symTable, snd_? = true, deref_? = true)
    }

  // Generates the assembly for a function call rvalue
  private def genCall(
      c: Call,
      symTable: SymbolTable[Dest],
      allocator: Allocator
  ): ListBuffer[Instruction] = {
    // Arguments are treated as a series of declarations
    val stmts: List[Stmt] = c.args.zipWithIndex.map { case (exp, i) =>
      Decl(exp.typ.get, Ident(s"_arg$i"), exp)
    }
    // New scope for the arguments
    genScopedStmt(
      stmts,
      c.args,
      symTable,
      allocator,
      ParamMode,
      lb(CallAsm(Label(s"wacc_${c.ident.name}")))
    ) += Push(Eax(Size64))
  }

  // Generates the assembly for an array literal
  private def genArray(
      a: ArrayLiter,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = {
    val typ = a.typ.get match {
      case ArrayType(t) => t
      case StringType   => CharType
      case NullType     => ArrayType(NullType)
      case _            => throw new IllegalArgumentException(s"Type ${a.typ.get} was not an array")
    }
    // Size of array elements
    val elemSize = Allocator.getTypeWidth(typ)
    // Memory to allocate for the array
    val size = intSize + a.elems.length * elemSize
    var position = -elemSize
    lb(
      Mov(size, Edi()),
      // malloc enough space for the array
      CallAsm(Label(s"_$malloc")),
      Mov(Eax(Size64), R11(Size64)),
      // Store the length of the array before the start of the array
      Mov(a.elems.length, Address(R11(Size64))),
      AddAsm(intSize, R11(Size64)),
      a.elems.flatMap { x =>
        position += elemSize
        lb(
          genExpr(x, symTable),
          Pop(Eax(Size64)),
          Mov(Eax(Size64), Address(R11(Size64), position))
        )
      },
      Push(R11(Size64))
    )
  }

  // Generates the assembly for a pair literal
  private def genPair(a: Expr, b: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] = lb(
    Mov(2 * ptrSize, Edi()),
    CallAsm(Label(s"_$malloc")),
    Mov(Eax(Size64), R11(Size64)),
    // Store the first element of the pair
    genExpr(a, symTable),
    Pop(Eax(Size64)),
    Mov(Eax(Size64), Address(R11(Size64))),
    // Store the second element of the pair
    genExpr(b, symTable),
    Pop(Eax(Size64)),
    Mov(Eax(Size64), Address(R11(Size64), ptrSize)),
    Push(R11(Size64))
  )

  // Generates the assembly for a pair element
  private def genPairElem(
    lval: LVal,
    symTable: SymbolTable[Dest],
    snd_? : Boolean = false,
    deref_? : Boolean = false
  ): ListBuffer[Instruction] = {
    val derefCheck = lb(
      // Check if the pair is null
      Cmp(nullPtr, Eax(Size64)),
      JmpComparison(Label(s"_$errNull"), Eq)
    )
    val deref = if (deref_?) Mov(Address(Eax(Size64)), Eax(Size64)) else lb()
    lval match {
      case Fst(value) =>
        // First element of pair is at the start of the pair
        genLVal(value, symTable, deref_? = true) ++=
          lb(Pop(Eax(Size64)), derefCheck, deref, Push(Eax(Size64)))
      case Snd(value) =>
        lb(
          genLVal(value, symTable, deref_? = true),
          Pop(Eax(Size64)),
          derefCheck,
          // Move to the second element of the pair
          AddAsm(ptrSize, Eax(Size64)),
          deref,
          Push(Eax(Size64))
        )
      case _ => throw new IllegalArgumentException(s"Fst or Snd expected, got: ${lval.getClass}")
    }
  }

  // Generates the assembly for a read statement
  private def genReadStmt(symTable: SymbolTable[Dest], lval: LVal): ListBuffer[Instruction] = {
    val call = lval.typ match {
      // Call the appropriate read function based on the type
      case Some(CharType) => CallAsm(Label(s"_$read$charType"))
      case Some(IntType)  => CallAsm(Label(s"_$read$intType"))
      case _ =>
        throw new IllegalArgumentException(s"Read called with unsupported type: ${lval.typ.get}")
    }
    lval match {
      case id: Ident =>
        lb(
          // Store the variable value in case of EOF
          Mov(symTable(id).get, Edi(Size64)),
          call,
          Mov(Eax(Size64), symTable(id).get)
        )
      case _ =>
        lb(
          genLVal(lval, symTable, deref_? = true),
          Pop(Ebx(Size64)),
          // Dereference the pointer to get value to store in case of EOF
          Mov(Address(Ebx(Size64)), Edi(Size64)),
          Push(Ebx(Size64)),
          call,
          Pop(Ebx(Size64)),
          Mov(Eax(Size64), Address(Ebx(Size64)))
        )
    }
  }

  // Generates the assembly for a print statement
  private def genPrintStmt(symTable: SymbolTable[Dest], expr: Expr): ListBuffer[Instruction] = {
    lb(
      genExpr(expr, symTable),
      Pop(Eax(Size64)),
      Mov(Eax(Size64), Edi(Size64)),
      // Call the appropriate print function based on the type
      expr.typ.get match {
        case CharType                             => CallAsm(Label(s"_$print$charType"))
        case IntType                              => CallAsm(Label(s"_$print$intType"))
        case StringType | ArrayType(CharType)     => CallAsm(Label(s"_$print$stringType"))
        case BoolType                             => CallAsm(Label(s"_$print$boolType"))
        case ArrayType(_) | PairType(_, _) | Pair => CallAsm(Label(s"_$print$ptrType"))
        case _ =>
          throw new IllegalArgumentException(s"Unsupported type: ${expr.typ.get}")
      }
    )
  }

  // Generates the assembly for calling exit
  private def genExit(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] = lb(
    genExpr(expr, symTable),
    Pop(Eax(Size64)),
    Mov(Eax(Size64), Edi(Size64)),
    CallAsm(Label(s"_$exit")),
    Mov(0, Eax())
  )

  // Generates the assembly for an lvalue
  private def genLVal(
    lval: LVal,
    symTable: SymbolTable[Dest],
    deref_? : Boolean = false
  ): ListBuffer[Instruction] = {
    lval match {
      case id: Ident               => lb(Push(symTable(id).get))
      case ArrayElem(ident, exprs) => genArrayElem(ident, exprs.init, symTable)
      // We may or may not need to dereference the pair, but recursive calls always do
      case f @ Fst(_)              =>
        genPairElem(f, symTable, deref_? = deref_?)
      case s @ Snd(_)              =>
        genPairElem(s, symTable, snd_? = true, deref_? = deref_?)
    }
  }

  // Generates the assembly for an array element
  private def genArrayElem(
      ident: Ident,
      exprs: List[Expr],
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = {
    val dest = symTable(ident).get
    var typ: Type = ident.typ.get
    // Load the address of the array
    val instructions = lb(Mov(dest, R9(Size64)))
    // Repeatedly call arrLoad until we reach the required nesting level
    for (expr <- exprs) {
      // Get the current type of the array, may change as we enter nested arrays
      typ = typ match {
        case ArrayType(t) => t
        case _            => throw new IllegalArgumentException(s"Type $typ was not an array")
      }
      val s = Allocator.getTypeWidth(typ)
      instructions ++= lb(
        Push(R9(Size64)),
        genExpr(expr, symTable),
        Pop(R10(Size64)),
        Pop(R9(Size64)),
        // Call the appropriate arrLoad function based on the size
        CallAsm(Label(s"_$arrLoad$s"))
      )
    }
    instructions += Push(R9(Size64))
  }

  // Generates the assembly for an expression
  private def genExpr(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] = lb(
    expr match {
      case Integer(i) => Mov(i, Eax())
      case StringAtom(s) =>
        Lea(Address(
          Rip, // String literals have " escaped
          Label(s".L.str${stringLiters(builtInFunctions.doubleEscape(s))}")),
          Eax(Size64))
      case Bool(value)  => Mov(if (value) 1 else 0, Eax(Size64))
      case Character(c) => Mov(c, Eax(Size64))
      case ArrayElem(ident, exprs) => genArrayElem(ident, exprs, symTable)
      case Ident(name) =>
        symTable(Ident(name)) match {
          case Some(value) => Mov(value, Eax(Size64))
          case None        => throw new NoSuchElementException(s"Variable $name not found")
        }
      case Null => Mov(nullPtr, Eax(Size64))
      case BinaryApp(op, left, right) => genBinaryApp(op, left, right, symTable)
      case UnaryApp(op, expr)         => genUnaryApp(op, expr, symTable)
      case BracketedExpr(expr) => genExpr(expr, symTable)
    },
    // If the expression is one of these, the result will already be pushed to the stack
    if (expr.isInstanceOf[BracketedExpr] || expr.isInstanceOf[ArrayElem]) lb()
    else Push(Eax(Size64))
  )

  // Generates the assembly for a binary operation
  private def genBinaryApp(
      op: BinaryOp,
      left: Expr,
      right: Expr,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = {
    val size = Allocator.getTypeSize(left.typ.get)
    lb(
      genExpr(right, symTable),
      genExpr(left, symTable),
      Pop(Eax(Size64)),
      Pop(Ebx(Size64)),
      op match {
        // Arithmetic operations that may overflow
        case Add | Sub | Mul => lb(
          op match {
            case Add => AddAsm(Ebx(Size32), Eax(Size32))
            case Sub => SubAsm(Ebx(Size32), Eax(Size32))
            case Mul => Imul(Ebx(Size32), Eax(Size32))
          },
          Jo(Label(s"_$errOverflow")),
          Movs(Eax(Size32), Eax(Size64), Size32, Size64)
        )
        // Comparison operations
        case Eq | NotEq | Gt | GtEq | Lt | LtEq => lb(
          Cmp(Ebx(size), Eax(size)),
          SetAsm(Eax(Size8), op.asInstanceOf[Comparison]),
          Movs(Eax(Size8), Eax(Size64), Size8, Size64)
        )
        // Division and modulo operations
        case Mod | Div => lb(
          Cmp(0, Ebx(Size32)),
          JmpComparison(Label(s"_$errDivZero"), Eq),
          Push(Edx(Size64)),
          Cltd,
          Idiv(Ebx(Size32)),
          if (op == Mod) Mov(Edx(Size32), Eax(Size32)) else lb(),
          Movs(Eax(Size32), Eax(Size64), Size32, Size64),
          Pop(Edx(Size64)) // Pop back
        )
        // Logical operations
        case Or | And =>
          val label = Allocator.allocateLabel
          lb(
            Cmp(boolTrue, Eax(Size64)),
            if (op == Or) JmpComparison(label, Eq) else JmpComparison(label, NotEq),
            Cmp(boolTrue, Ebx(Size64)),
            label,
            SetAsm(Eax(Size8), Eq),
            Movs(Eax(Size8), Eax(Size64), Size8, Size64)
          )
      }
    )
  }

  // Generates the assembly for a unary operation
  private def genUnaryApp(
      op: UnaryOp,
      expr: Expr,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = lb(
    genExpr(expr, symTable),
    Pop(Eax(Size64)),
    op match {
      case Chr => lb(
        Movs(Eax(Size8), Eax(Size64), Size8, Size64),
        // Check if the character is in the valid range
        Testq(badChar, Eax(Size64)),
        CMovne(Eax(Size64), Esi(Size64)),
        JmpComparison(Label(s"_$errBadChar"), NotEq)
      )
      // Arrays have their length stored at the address before the start of the array
      case Len => Mov(Address(Eax(Size64), -intSize), Eax())
      case Neg => lb(
        Mov(0, Edx(Size64)),
        SubAsm(Eax(Size32), Edx(Size32)),
        Jo(Label(s"_$errOverflow")),
        Movs(Edx(Size32), Eax(Size64), Size32, Size64)
      )
      case Not => lb(
        Cmp(boolTrue, Eax(Size64)),
        SetAsm(Eax(Size8), NotEq),
        Movs(Eax(Size8), Eax(Size64), Size8, Size64)
      )
      case Ord => lb( // Sign extend the character
        Movs(Eax(Size8), Eax(Size64), Size8, Size64)
      )
    }
  )
}
