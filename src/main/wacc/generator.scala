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
    .mkString("\n")

  private def genProgram(program: Program): ListBuffer[Instruction] = {
    val instructions = lb(
      Directive("globl main"),
      genDataSection(stringLiters.view.mapValues(i => s".L.str$i").toSeq: _*),
      program.functions.map(x => genFunc(x, SymbolTable(None), Allocator(x.vars))),
      Label("main")
    )

    val mainSymTable: SymbolTable[Dest] = SymbolTable(None)
    val allocator = Allocator(program.vars)
    val mainBody = lb(
      genStmts(program.body, mainSymTable, allocator),
      Mov(Immediate(0), Eax(Size64))
    )

    instructions ++= lb(
      genNewScopeEnter(program.vars, allocator.reservedSpace, mainSymTable),
      mainBody,
      genNewScopeExit(program.vars, allocator.reservedSpace, mainSymTable),
      Ret,
      genFunctions
    )
  }

  private def genFunc(
      func: Func,
      symTable: SymbolTable[Dest],
      allocator: Allocator
  ): ListBuffer[Instruction] = ListBuffer.empty // TODO

  private def genStmts(
      stmts: ListBuffer[Stmt],
      symTable: SymbolTable[Dest],
      allocator: Allocator
  ): ListBuffer[Instruction] =
    lb(
      stmts.flatMap(x => {
        println(symTable.table)
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
      case Print(expr)           => genPrintStmt(symTable, expr)
      case PrintLn(expr)         => genPrintStmt(symTable, expr) += CallAsm(Label("_printn"))
      case Read(lval)            => genReadStmt(symTable, lval)
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
    val IfStmt(expr: Expr, body1: List[Stmt], body2: List[Stmt]) = ifStmt
    println(symTable.table)
    println("yeahh")
    lb(
      genExpr(expr, symTable), {
        val labelTrue = Allocator.allocateLabel
        val labelContinue = Allocator.allocateLabel

        val childSymTable = symTable.makeChild
        println("after make child:")
        println(symTable.table)

        lb(
          Pop(Eax(Size64)),
          Cmp(Immediate(1), Eax(Size64)),
          JmpComparison(labelTrue, Eq),
          genNewScopeEnter(ifStmt.branch2Vars, allocator.reservedSpace, symTable),
          // genNewScope(genStmts(body2, childSymTable, allocator), ifStmt.branch2Vars, symTable),
          genStmts(body2, childSymTable, allocator),
          genNewScopeExit(ifStmt.branch2Vars, allocator.reservedSpace, symTable),
          Jmp(labelContinue),
          labelTrue,
          genNewScopeEnter(ifStmt.branch2Vars, allocator.reservedSpace, symTable),
          genStmts(body1, childSymTable, allocator),

          // genNewScope(genStmts(body1, childSymTable, allocator), ifStmt.branch1Vars, symTable),
          genNewScopeExit(ifStmt.branch2Vars, allocator.reservedSpace, symTable),
          labelContinue
        )
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
    val dest = lval match {
      case id: Ident => symTable(id)
      case _         => ???
    }
    lb(
      genRval(value, symTable),
      Pop(Eax(Size64)),
      Mov(Eax(Size64), dest.get)
    )
  }

  private def genFreeStmt(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] = lb(
    genExpr(expr, symTable),
    Pop(Eax(Size64)),
    Mov(Eax(Size64), Edi(Size64)),
    SubAsm(Immediate(4), Edi(Size64)),
    CallAsm(Label("_free"))
  )

  private def genRval(value: RVal, symTable: SymbolTable[Dest]): ListBuffer[Instruction] =
    value match {
      case e: Expr       => genExpr(e, symTable)
      case a: ArrayLiter => genArray(value.typ.get, a, symTable)
    }

  private def genArray(
      t: Type,
      a: ArrayLiter,
      symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = {
    val ArrayType(typ) = t
    val elemSize = Allocator.getTypeWidth(typ)
    val size = intSize + a.elems.length * elemSize
    var position = -elemSize
    lb(
      Mov(Immediate(size), Edi()),
      CallAsm(Label("_malloc")),
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
    lb(
      genLVal(lval, symTable),
      Mov(Eax(Size64), Edi(Size64)),
      lval match {
        case id: Ident =>
          id.typ.get match {
            case CharType => CallAsm(Label("_readc"))
            case IntType  => CallAsm(Label("_readi"))
          }
        case _ => ???
      }
    )
  }

  private def genPrintStmt(symTable: SymbolTable[Dest], expr: Expr): ListBuffer[Instruction] = {
    lb(
      genExpr(expr, symTable),
      Pop(Eax(Size64)),
      Mov(Eax(Size64), Edi(Size64)),
      expr.typ.get match {
        case CharType                             => CallAsm(Label("_printc"))
        case IntType                              => CallAsm(Label("_printi"))
        case StringType | ArrayType(CharType)     => CallAsm(Label("_prints"))
        case BoolType                             => CallAsm(Label("_printb"))
        case ArrayType(_) | PairType(_, _) | Pair => CallAsm(Label("_printp"))
        case _                                    => println(expr.typ.get)
      }
    )
  }

  private def genExit(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] =
    lb(
      genExpr(expr, symTable),
      Pop(Eax(Size64)),
      Mov(Eax(Size64), Edi(Size64)),
      CallAsm(Label("_exit")),
      Mov(Immediate(0), Eax())
    )

  private def genLVal(lval: LVal, symTable: SymbolTable[Dest]) = ???

  private def genExpr(expr: Expr, symTable: SymbolTable[Dest]): ListBuffer[Instruction] = lb(
    expr match {
      case Integer(i)    => Mov(Immediate(i.toLong), Eax())
      case StringAtom(s) => Lea(Address(Rip, Label(s".L.str${stringLiters(s)}")), Eax(Size64))
      case Bool(value)   => Mov(Immediate(if (value) 1 else 0), Eax(Size64))
      case Character(c)  => Mov(Immediate(c.toLong), Eax(Size64))

      case ArrayElem(ident, exprs) => ???
      case Ident(name) =>
        symTable(Ident(name)) match {
          case Some(value) => Mov(value, Eax(Size64))
          case None        => ???
        }
      case Null => Mov(Immediate(0), Eax(Size64))

      case BinaryApp(op, left, right) => genBinaryApp(op, left, right, symTable)
      case UnaryApp(op, expr)         => genUnaryApp(op, expr, symTable)

      case BracketedExpr(expr) => genExpr(expr, symTable)

    },

    // If the expression is a bracketed expression, the result will already be pushed to the stack
    if (expr.isInstanceOf[BracketedExpr]) lb() else Push(Eax(Size64))
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
          Jo(Label("_errOverflow")),
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
          JmpComparison(Label("_errDivZero"), Eq),
          // As Cltd will write into edx?? This isn't in reference compiler I just did it.
          Push(Edx(Size64)),
          Cltd,
          Idiv(Ebx(Size32)),
          if (op == Mod) Mov(Edx(Size32), Eax(Size32)) else lb(),
          Movs(Eax(Size32), Eax(Size64)),
          Pop(Edx(Size64)) // Pop back
        )

      case Or | And => {
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
          Cmovne(Eax(Size64), Esi(Size64)),
          JmpComparison(Label("_errBadChar"), NotEq)
        )
      case Len => ???
      case Neg =>
        // slightly different to how reference compiler does it, as we assume the answer of exp is stored in Eax
        lb(
          Mov(Immediate(0), Edx(Size64)),
          SubAsm(Eax(Size32), Edx(Size32)),
          Jo(Label("_errOverflow")),
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
