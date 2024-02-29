package src.main.wacc

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import src.main.wacc.constants._
import src.main.wacc.builtInFunctions._

object generator {

  def lb(instructions: Any*): ListBuffer[Instruction] = {
    val resultBuffer = ListBuffer[Instruction]()

    for (instruction <- instructions) {
      instruction match {
        case inst: Instruction =>
          resultBuffer += inst
        case instList: List[_] if instList.forall(_.isInstanceOf[Instruction]) =>
          resultBuffer ++= instList.asInstanceOf[List[Instruction]]
        case instBuffer: ListBuffer[_] if instBuffer.forall(_.isInstanceOf[Instruction]) =>
          resultBuffer ++= instBuffer.asInstanceOf[ListBuffer[Instruction]]
        case _ =>
          throw new IllegalArgumentException(s"Unsupported type: ${instruction.getClass}")
      }
    }

    resultBuffer
  }

  val stringLiters: mutable.Map[String, Int] = mutable.Map.empty

  def generate(program: Program, formatter: Formatter): String = genProgram(program)
    .map(formatter(_))
    .mkString("\n") + "\n"

  private def genProgram(program: Program): ListBuffer[Instruction] = {
    val instructions = lb(
      dirGlobl,
      genDataSection(stringLiters.view.mapValues(i => s".L.str$i").toSeq: _*),
      mainLabel
    )

    val mainSymTable: SymbolTable[Dest] = SymbolTable(None)
    val allocator = Allocator(program.vars)

    val mainBody = lb(
      genStmts(program.body, mainSymTable, allocator),
      Mov(exitSuccess, Eax(Size64)),
    )

    val savedRegs = Allocator.NON_PARAM_REGS.take(program.vars.size)
    symTableEnterScope(mainSymTable, allocator, savedRegs)
    instructions ++= lb(
      genNewScopeEnter(program.vars),
      mainBody,
      genNewScopeExit(program.vars),
      Ret,
      program.functions.flatMap(x => genFunc(x)),
      genFunctions
    )
    symTableExitScope(mainSymTable, allocator, savedRegs)
    instructions
  }

  private def genFunc(
      func: Func
  ): ListBuffer[Instruction] = {
    val allocator = Allocator(
      func.params.drop(Allocator.PARAM_REGS.length).map(x => Allocator.getTypeWidth(x.t)).sum,
      ParamMode
    )
    val paramTable = SymbolTable[Dest](None)

    func.params.foreach { param =>
      paramTable.put(param.ident, allocator.allocateSpace(param.t))
    }

    val usedParamRegs = Allocator.PARAM_REGS.take(func.params.length)

    symTableEnterScope(paramTable, allocator, usedParamRegs, ParamMode)

    val exitScope = genNewScopeExit(usedParamRegs, func.vars)

    val toAllocate = func.vars.drop(Allocator.NON_PARAM_REGS.length)

    val instructions = lb(
      Label(s"wacc_${func.ident.name}"),
      genNewScopeEnter(usedParamRegs, toAllocate),
      genStmts(func.body, paramTable.makeChild, Allocator(toAllocate, NonParamMode), exitScope)
    )

    symTableExitScope(paramTable, allocator, usedParamRegs, ParamMode)

    instructions
  }

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

  private def genStmt(
      stmt: Stmt,
      symTable: SymbolTable[Dest],
      allocator: Allocator,
      exitScope: ListBuffer[Instruction] = lb()
  ): ListBuffer[Instruction] =
    stmt match {
      case Skip       => lb()
      case Exit(expr) => genExit(expr, symTable)
      case Return(expr) =>
        lb(
          genExpr(expr, symTable),
          Pop(Eax(Size64)),
          exitScope,
          Ret
        )
      case Print(expr)   => genPrintStmt(symTable, expr)
      case PrintLn(expr) => genPrintStmt(symTable, expr) += CallAsm(Label(s"_$print$printlnType"))
      case Read(lval)    => genReadStmt(symTable, lval)
      case Decl(t, ident, value) =>
        // We can allocate the register before we generate rval as the stack machine will
        // only use %eax and %ebx, which are protected
        val dest = allocator.allocateSpace(t)
        symTable.put(ident, dest)
        genDeclStmt(value, dest, symTable, allocator)
      case Asgn(lval, value) => genAsgnStmt(lval, value, symTable, allocator)
      case Free(expr)        => genFreeStmt(expr, symTable)
      case ifStmt: IfStmt =>
        genIfStmt(ifStmt, symTable, allocator, exitScope) // handle IfStmt case
      case s @ ScopedStmt(stmts) =>
        genScopedStmt(stmts, s.vars, symTable, allocator, NonParamMode, lb(), exitScope)
      case w @ While(expr, stmts) => genWhile(expr, stmts, w.vars, symTable, allocator, exitScope)
    }

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
      Jmp(labelExpr),
      labelStmts,
      genScopedStmt(stmts, vars, symTable, allocator, NonParamMode, lb(), exitScope),
      labelExpr,
      generatedExpr,
      Pop(Eax(Size64)),
      Cmp(boolTrue, Eax(Size64)),
      JmpComparison(labelStmts, Eq)
    )
  }

  private def genIfStmt(
      ifStmt: IfStmt,
      symTable: SymbolTable[Dest],
      allocator: Allocator,
      exitScope: ListBuffer[Instruction] = lb()
  ): ListBuffer[Instruction] = {
    val IfStmt(expr, body1, body2) = ifStmt
    lb(
      genExpr(expr, symTable), {
        val labelTrue = Allocator.allocateLabel
        val labelContinue = Allocator.allocateLabel

        lb(
          Pop(Eax(Size64)),
          Movs(Eax(Size8), Eax(Size64), Size8, Size64),
          Cmp(Immediate(1), Eax(Size64)),
          JmpComparison(labelTrue, Eq),
          genScopedStmt(
            body2,
            ifStmt.branch2Vars,
            symTable,
            allocator,
            NonParamMode,
            lb(),
            exitScope
          ),
          Jmp(labelContinue),
          labelTrue,
          genScopedStmt(
            body1,
            ifStmt.branch1Vars,
            symTable,
            allocator,
            NonParamMode,
            lb(),
            exitScope
          ),
          labelContinue
        )

      }
    )

  }

  private def genDeclStmt(
      value: RVal,
      dest: Dest,
      symTable: SymbolTable[Dest],
      allocator: Allocator
  ): ListBuffer[Instruction] = lb(
    genRval(value, symTable, allocator),
    Pop(Eax(Size64)),
    Mov(
      Eax(if (dest.isInstanceOf[Reg]) Size64 else Allocator.getTypeSize(value.typ.get)),
      dest,
      useOpSize = true
    )
  )

  private def genAsgnStmt(
      lval: LVal,
      value: RVal,
      symTable: SymbolTable[Dest],
      allocator: Allocator
  ): ListBuffer[Instruction] = {
    lval match {
      case id: Ident =>
        lb(
          genRval(value, symTable, allocator),
          Pop(Eax(Size64)),
          Mov(
            Eax(
              if (symTable(id).get.isInstanceOf[Reg]) Size64
              else Allocator.getTypeSize(value.typ.get)
            ),
            symTable(id).get,
            useOpSize = true
          )
        )
      case arr @ ArrayElem(ident, exprs) =>
        var typ = ident.typ.get
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
          CallAsm(Label(s"_$arrStore${Allocator.getTypeWidth(typ)}"))
        )
      case Fst(f) => lb(
          genLVal(f, symTable),
          genRval(value, symTable, allocator),
          Pop(Eax(Size64)),
          Pop(Ebx(Size64)),
          Mov(Eax(Size64), Address(Ebx(Size64)))
        )
      case Snd(s) => lb(
          genLVal(s, symTable),
          genRval(value, symTable, allocator),
          Pop(Eax(Size64)),
          Pop(Ebx(Size64)),
          Mov(Eax(Size64), Address(Ebx(Size64), Immediate(ptrSize)))
        )
    }
  }

  private def genFreeStmt(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] =
    expr.typ.get match {
      case ArrayType(_) => lb(
        genExpr(expr, symTable),
        Pop(Eax(Size64)),
        Mov(Eax(Size64), Edi(Size64)),
        SubAsm(Immediate(4), Edi(Size64)),
        CallAsm(Label(s"_$free"))
      )
      case PairType(_, _) | Pair => lb(
        genExpr(expr, symTable),
        Pop(Eax(Size64)),
        Mov(Eax(Size64), Edi(Size64)),
        CallAsm(Label(s"_$freepair"))
      )
    }

  private def genRval(
      value: RVal,
      symTable: SymbolTable[Dest],
      allocator: Allocator
  ): ListBuffer[Instruction] =
    value match {
      case e: Expr       => genExpr(e, symTable)
      case a: ArrayLiter => genArray(value.typ.get, a, symTable)
      case c: Call       => genCall(c, symTable, allocator)
      case NewPair(a, b) => genPair(a, b, symTable)
      case f @ Fst(_)    => genPairElem(f, symTable) ++=
        lb(Pop(Eax(Size64)), Mov(Address(Eax(Size64)), Eax(Size64)), Push(Eax(Size64)))
      case s @ Snd(_)    => genPairElem(s, symTable, snd_? = true) ++=
        lb(Pop(Eax(Size64)), Mov(Address(Eax(Size64)), Eax(Size64)), Push(Eax(Size64)))
    }

  private def genCall(
      c: Call,
      symTable: SymbolTable[Dest],
      allocator: Allocator
  ): ListBuffer[Instruction] = {
    val stmts: List[Stmt] = c.args.zipWithIndex.map { case (exp, i) =>
      Decl(exp.typ.get, Ident(s"_arg$i"), exp)
    }

    genScopedStmt(
      stmts,
      c.args,
      symTable,
      allocator,
      ParamMode,
      lb(CallAsm(Label(s"wacc_${c.ident.name}")))
    ) += Push(Eax(Size64))
  }

  private def genArray(
      t: Type,
      a: ArrayLiter,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = {
    val typ = t match {
      case ArrayType(t) => t
      case StringType   => CharType
      case _            => throw new IllegalArgumentException(s"Type $t was not an array")
    }
    val elemSize = Allocator.getTypeWidth(typ)
    val size = intSize + a.elems.length * elemSize
    var position = -elemSize
    lb(
      Mov(Immediate(size), Edi()),
      CallAsm(Label(s"_$malloc")),
      Mov(Eax(Size64), R11(Size64)),
      Mov(Immediate(a.elems.length), Address(R11(Size64))),
      AddAsm(Immediate(intSize), R11(Size64)),
      a.elems.flatMap { x =>
        position += elemSize
        lb(
          genExpr(x, symTable),
          Pop(Eax(Size64)),
          Mov(Eax(Size64), Address(R11(Size64), Immediate(position)))
        )
      },
      Push(R11(Size64))
    )
  }

  private def genPair(a: Expr, b: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] = lb(
    Mov(Immediate(2 * ptrSize), Edi()),
    CallAsm(Label(s"_$malloc")),
    Mov(Eax(Size64), R11(Size64)),
    genExpr(a, symTable),
    Pop(Eax(Size64)),
    Mov(Eax(Size64), Address(R11(Size64))),
    genExpr(b, symTable),
    Pop(Eax(Size64)),
    Mov(Eax(Size64), Address(R11(Size64), Immediate(ptrSize))),
    Push(R11(Size64))
  )

  private def genPairElem(
      lval: LVal,
      symTable: SymbolTable[Dest],
      snd_? : Boolean = false
  ): ListBuffer[Instruction] = lval match {
    case Fst(value) => genLVal(value, symTable)
    case Snd(value) => lb(
      genLVal(value, symTable),
      Pop(Eax(Size64)),
      AddAsm(Immediate(ptrSize), Eax(Size64)),
      Push(Eax(Size64))
    )
    case _ => throw new IllegalArgumentException(s"Fst or Snd expected, got: ${lval.getClass}")
  }

  private def genReadStmt(symTable: SymbolTable[Dest], lval: LVal): ListBuffer[Instruction] = {
    val call = lval.typ match {
      case Some(CharType) => CallAsm(Label(s"_$read$charType"))
      case Some(IntType)  => CallAsm(Label(s"_$read$intType"))
      case _ =>
        throw new IllegalArgumentException(s"Read called with unsupported type: ${lval.typ.get}")
    }
    lval match {
      case id: Ident =>
        lb(
          Mov(symTable(id).get, Edi(Size64)),
          call,
          Mov(Eax(Size64), symTable(id).get)
        )
      case _ =>
        lb(
          genLVal(lval, symTable),
          Pop(Ebx(Size64)),
          Mov(Address(Ebx(Size64)), Edi(Size64)),
          Push(Ebx(Size64)),
          call,
          Pop(Ebx(Size64)),
          Mov(Eax(Size64), Address(Ebx(Size64)))
        )
    }
  }

  private def genPrintStmt(symTable: SymbolTable[Dest], expr: Expr): ListBuffer[Instruction] = {
    lb(
      genExpr(expr, symTable),
      Pop(Eax(Size64)),
      Mov(Eax(Size64), Edi(Size64)),
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

  private def genExit(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] = lb(
    genExpr(expr, symTable),
    Pop(Eax(Size64)),
    Mov(Eax(Size64), Edi(Size64)),
    CallAsm(Label(s"_$exit")),
    Mov(Immediate(0), Eax())
  )

  private def genLVal(lval: LVal, symTable: SymbolTable[Dest]): ListBuffer[Instruction] =
    lval match {
      case id: Ident               => lb(Push(symTable(id).get))
      case ArrayElem(ident, exprs) => genArrayElem(ident, exprs.init, symTable)
      case f @ Fst(_)              => genPairElem(f, symTable)
      case s @ Snd(_)              => genPairElem(s, symTable, snd_? = true)
    }

  private def genArrayElem(
      ident: Ident,
      exprs: List[Expr],
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = {
    val dest = symTable(ident).get
    var typ: Type = ident.typ.get
    val instructions = lb(Mov(dest, R9(Size64)))
    for (expr <- exprs) {
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
        CallAsm(Label(s"_$arrLoad$s"))
      )
    }
    instructions += Push(R9(Size64))
  }

  private def genExpr(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] = lb(
    expr match {
      case Integer(i) => Mov(Immediate(i), Eax())
      case StringAtom(s) =>
        Lea(Address(Rip, Label(s".L.str${stringLiters(s.replace("\"", "\\\""))}")), Eax(Size64))
      case Bool(value)  => Mov(Immediate(if (value) 1 else 0), Eax(Size64))
      case Character(c) => Mov(Immediate(c.toInt), Eax(Size64))

      case ArrayElem(ident, exprs) => genArrayElem(ident, exprs, symTable)
      case Ident(name) =>
        symTable(Ident(name)) match {
          case Some(value) => Mov(value, Eax(Size64))
          case None        => throw new NoSuchElementException(s"Variable $name not found")
        }
      case Null => Mov(Immediate(0), Eax(Size64))

      case BinaryApp(op, left, right) => genBinaryApp(op, left, right, symTable)
      case UnaryApp(op, expr)         => genUnaryApp(op, expr, symTable)

      case BracketedExpr(expr) => genExpr(expr, symTable)

    },

    // If the expression is a bracketed expression, the result will already be pushed to the stack
    if (expr.isInstanceOf[BracketedExpr] || expr.isInstanceOf[ArrayElem])
      lb()
    else Push(Eax(Size64))
  )

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
        case Add | Sub | Mul => lb(
          op match {
            case Add => AddAsm(Ebx(Size32), Eax(Size32))
            case Sub => SubAsm(Ebx(Size32), Eax(Size32))
            case Mul => Imul(Ebx(Size32), Eax(Size32))
          },
          Jo(Label(s"_$errOverflow")),
          Movs(Eax(Size32), Eax(Size64), Size32, Size64)
        )
        case Eq | NotEq | Gt | GtEq | Lt | LtEq => lb(
          Cmp(Ebx(size), Eax(size)),
          SetAsm(Eax(Size8), op.asInstanceOf[Comparison]),
          Movs(Eax(Size8), Eax(Size64), Size8, Size64)
        )
        case Mod | Div => lb(
          Cmp(Immediate(0), Ebx(Size32)),
          JmpComparison(Label(s"_$errDivZero"), Eq),
          // As Cltd will write into edx?? This isn't in reference compiler I just did it.
          Push(Edx(Size64)),
          Cltd,
          Idiv(Ebx(Size32)),
          if (op == Mod) Mov(Edx(Size32), Eax(Size32)) else lb(),
          Movs(Eax(Size32), Eax(Size64), Size32, Size64),
          Pop(Edx(Size64)) // Pop back
        )
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

  private def genUnaryApp(
      op: UnaryOp,
      expr: Expr,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = lb(
    genExpr(expr, symTable),
    Pop(Eax(Size64)),
    op match {
      case Chr =>
        lb(
          Movs(Eax(Size8), Eax(Size64), Size8, Size64),
          Testq(badChar, Eax(Size64)),
          CMovne(Eax(Size64), Esi(Size64)),
          JmpComparison(Label(s"_$errBadChar"), NotEq)
        )
      case Len =>
        lb(
          Mov(Address(Eax(Size64), Immediate(-intSize)), Eax())
        )
      case Neg =>
        lb(
          Mov(Immediate(0), Edx(Size64)),
          SubAsm(Eax(Size32), Edx(Size32)),
          Jo(Label(s"_$errOverflow")),
          Movs(Edx(Size32), Eax(Size64), Size32, Size64)
        )
      case Not =>
        lb(
          Cmp(boolTrue, Eax(Size64)),
          SetAsm(Eax(Size8), NotEq),
          Movs(Eax(Size8), Eax(Size64), Size8, Size64)
        )
      case Ord =>
        lb(Movs(Eax(Size8), Eax(Size64), Size8, Size64)) // Do nothing as char already being stored as a Long in eax
    }
  )
}
