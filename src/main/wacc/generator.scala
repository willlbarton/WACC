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
      program.functions.map(x => genFunc(x, SymbolTable(None), Allocator(x.vars))),
      mainLabel
    )

    val mainSymTable: SymbolTable[Dest] = SymbolTable(None)
    val allocator = Allocator(program.vars)
    val mainBody = lb(
      genStmts(program.body, mainSymTable, allocator),
      Mov(Immediate(0), Eax(Size64))
    )

    val savedRegs = Allocator.NON_PARAM_REGS.take(program.vars.size)
    symTableEnterScope(mainSymTable, allocator, savedRegs)
    instructions ++= lb(
      genNewScopeEnter(program.vars),
      mainBody,
      genNewScopeExit(program.vars),
      Ret,
      genFunctions
    )
    symTableExitScope(mainSymTable, allocator, savedRegs)
    instructions
  }

  private def genFunc(
      func: Func,
      symTable: SymbolTable[Dest],
      allocator: Allocator
  ): ListBuffer[Instruction] = ??? // TODO

  private def genStmts(
      stmts: ListBuffer[Stmt],
      symTable: SymbolTable[Dest],
      allocator: Allocator
  ): ListBuffer[Instruction] =
    lb(
      stmts.flatMap(x => {
        genStmt(x, symTable, allocator)
      })
    )

  private def genStmts(
      stmts: List[Stmt],
      symTable: SymbolTable[Dest],
      allocator: Allocator
  ): ListBuffer[Instruction] =
    genStmts(
      ListBuffer() ++= stmts,
      symTable,
      allocator
    )

  private def genStmt(
      stmt: Stmt,
      symTable: SymbolTable[Dest],
      allocator: Allocator
  ): ListBuffer[Instruction] =
    stmt match {
      case Skip       => lb()
      case Exit(expr) => genExit(expr, symTable)
      case Return(expr) =>
        lb(
          genExpr(expr, symTable),
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
        genDeclStmt(value, dest, symTable)
      case Asgn(lval, value) => genAsgnStmt(lval, value, symTable)
      case Free(expr)        => genFreeStmt(expr, symTable)
      case ifStmt: IfStmt =>
        genIfStmt(ifStmt, symTable, allocator) // handle IfStmt case
      case ScopedStmt(_) => ??? // handle ScopedStmt case
      case While(_, _)   => ??? // handle While case
    }

  private def genIfStmt(
      ifStmt: IfStmt,
      symTable: SymbolTable[Dest],
      allocator: Allocator
  ): ListBuffer[Instruction] = {
    val IfStmt(expr, body1, body2) = ifStmt
    lb(
      genExpr(expr, symTable), {
        val labelTrue = Allocator.allocateLabel
        val labelContinue = Allocator.allocateLabel

        val childSymTable = symTable.makeChild

        val savedRegs = Allocator.NON_PARAM_REGS.take(ifStmt.branch1Vars.size)
        symTableEnterScope(symTable, allocator, savedRegs)

        val instructions = lb(
          Pop(Eax(Size64)),
          Cmp(Immediate(1), Eax(Size64)),

          JmpComparison(labelTrue, Eq),
          genNewScopeEnter(ifStmt.branch2Vars),
          genStmts(body2, childSymTable, allocator),
          genNewScopeExit(ifStmt.branch2Vars),
          Jmp(labelContinue),

          labelTrue,
          genNewScopeEnter(ifStmt.branch1Vars),
          genStmts(body1, childSymTable, allocator),
          genNewScopeExit(ifStmt.branch1Vars),
          labelContinue
        )

        symTableExitScope(symTable, allocator, savedRegs)

        instructions
      }
    )

  }

  private def genDeclStmt(
      value: RVal,
      dest: Dest,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = lb(
    genRval(value, symTable),
    Pop(Eax(Size64)),
    Mov(Eax(Size64), dest)
  )

  private def genAsgnStmt(
      lval: LVal,
      value: RVal,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = {
    lval match {
      case id: Ident => lb(
        genRval(value, symTable),
        Pop(Eax(Size64)),
        Mov(Eax(Size64), symTable(id).get)
      )
      case arr@ArrayElem(ident, exprs) =>
        var typ = ident.typ.get
        typ = exprs.foldLeft(typ) { case (t, _) => t match {
          case ArrayType(t) => t
          case _ => throw new IllegalArgumentException(s"Type $t was not an array")
        } }
        lb(
          genLVal(arr, symTable),
          genExpr(exprs.last, symTable),
          genRval(value, symTable),
          Pop(Eax(Size64)),
          Pop(R10(Size64)),
          Pop(R9(Size64)),
          CallAsm(Label(s"_$arrStore${Allocator.getTypeWidth(typ)}"))
        )
      case _ => ???
    }
  }

  private def genFreeStmt(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] = lb(
    genExpr(expr, symTable),
    Pop(Eax(Size64)),
    Mov(Eax(Size64), Edi(Size64)),
    SubAsm(Immediate(4), Edi(Size64)),
    CallAsm(Label(s"_$free"))
  )

  private def genRval(value: RVal, symTable: SymbolTable[Dest]): ListBuffer[Instruction] =
    value match {
      case e: Expr       => genExpr(e, symTable)
      case a: ArrayLiter => genArray(value.typ.get, a, symTable)
      case _             => ???
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

  private def genReadStmt(symTable: SymbolTable[Dest], lval: LVal): ListBuffer[Instruction] = {
    val call = lval.typ match {
      case Some(CharType) => CallAsm(Label(s"_$read$charType"))
      case Some(IntType)  => CallAsm(Label(s"_$read$intType"))
      case _              => throw new
          IllegalArgumentException(s"Read called with unsupported type: ${lval.typ.get}")
    }
    lval match {
      case id: Ident => lb(
        call,
        Cmp(Immediate(-1), Eax(Size64)),
        CMovne(Eax(Size64), symTable(id).get)
      )
      case _         => lb(
        genLVal(lval, symTable),
        call,
        Pop(Ebx(Size64)),
        Cmp(Immediate(-1), Eax(Size64)),
        CMovne(Eax(Size64), Address(Ebx(Size64)))
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
        case _                                    =>
          throw new IllegalArgumentException(s"Unsupported type: ${expr.typ.get}")
      }
    )
  }

  private def genExit(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] =
    lb(
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
      case _                       => ???
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
        case _ => throw new IllegalArgumentException(s"Type $typ was not an array")
      }
      val s = Allocator.getTypeWidth(typ)
      instructions ++= lb(
        genExpr(expr, symTable),
        Pop(R10(Size64)),
        CallAsm(Label(s"_$arrLoad$s"))
      )
    }
    instructions += Push(R9(Size64))
  }

  private def genExpr(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] = lb(
    expr match {
      case Integer(i)    => Mov(Immediate(i), Eax())
      case StringAtom(s) => Lea(Address(Rip, Label(s".L.str${stringLiters(s)}")), Eax(Size64))
      case Bool(value)   => Mov(Immediate(if (value) 1 else 0), Eax(Size64))
      case Character(c)  => Mov(Immediate(c.toInt), Eax(Size64))

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
      lb() else Push(Eax(Size64))
  )

  private def genBinaryApp(
      op: BinaryOp,
      left: Expr,
      right: Expr,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = lb(
    genExpr(right, symTable),
    genExpr(left, symTable),
    Pop(Eax(Size64)),
    Pop(Ebx(Size64)),
    op match {
      case Add | Sub | Mul =>
        lb(
          op match {
            case Add => AddAsm(Ebx(Size32), Eax(Size32))
            case Sub => SubAsm(Ebx(Size32), Eax(Size32))
            case Mul => Imul(Ebx(Size32), Eax(Size32))
          },
          Jo(Label(s"_$errOverflow")),
          Movs(Eax(Size32), Eax(Size64))
        )

      case Eq | NotEq | Gt | GtEq | Lt | LtEq =>
        lb(
          Cmp(Ebx(Size64), Eax(Size64)),
          SetAsm(Eax(Size8), op.asInstanceOf[Comparison]),
          Movs(Eax(Size8), Eax(Size64))
        )
      case Mod | Div =>
        lb(
          Cmp(Immediate(0), Ebx(Size32)),
          JmpComparison(Label(s"_$errDivZero"), Eq),
          // As Cltd will write into edx?? This isn't in reference compiler I just did it.
          Push(Edx(Size64)),
          Cltd,
          Idiv(Ebx(Size32)),
          if (op == Mod) Mov(Edx(Size32), Eax(Size32)) else lb(),
          Movs(Eax(Size32), Eax(Size64)),
          Pop(Edx(Size64)) // Pop back
        )

      case Or | And =>
        val label = Allocator.allocateLabel
        lb(
          Cmp(Immediate(1), Eax(Size64)),
          if (op == Or) JmpComparison(label, Eq) else JmpComparison(label, NotEq),
          Cmp(Immediate(1), Ebx(Size64)),
          label,
          SetAsm(Eax(Size8), Eq),
          Movs(Eax(Size8), Eax(Size64))
        )
    }
  )

  private def genUnaryApp(
      op: UnaryOp,
      expr: Expr,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = lb(
    genExpr(expr, symTable),
    op match {
      case Chr =>
        lb(
          Testq(Immediate(-128), Eax(Size64)),
          CMovne(Eax(Size64), Esi(Size64)),
          JmpComparison(Label(s"_$errBadChar"), NotEq)
        )
      case Len => ???
      case Neg =>
        // slightly different to how reference compiler does it, as we assume the answer of exp is stored in Eax
        lb(
          Mov(Immediate(0), Edx(Size64)),
          SubAsm(Eax(Size32), Edx(Size32)),
          Jo(Label(s"_$errOverflow")),
          Movs(Edx(Size32), Eax(Size64))
        )
      case Not =>
        lb(
          Cmp(Immediate(1), Eax(Size64)),
          SetAsm(Eax(Size8), NotEq),
          Movs(Eax(Size8), Eax(Size64))
        )
      // Do nothing as char already being stored as a Long in eax
      case Ord => lb()
    }
  )
}
