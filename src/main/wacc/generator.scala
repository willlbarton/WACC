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
      program.body.flatMap(x => genStmt(x, mainSymTable, allocator)),
      Mov(Immediate(0), Eax(Size64))
    )

    instructions ++= lb(genNewScope(mainBody, program.vars), Ret, Label("_exit"))

    val exitBody: ListBuffer[Instruction] = lb(
      // Align stack pointer to 16 bytes
      AndAsm(Immediate(-16), Rsp),
      CallAsm(Label("exit@plt"))
    )
    instructions ++= lb(
      genNewScope(exitBody),
      genPrint(stringType, "%.*s"),
      genPrint(intType, "%d"),
      genPrint(printlnType, ""),
      genPrint(charType, "%c"),
      genPrint(ptrType, "%p"),
      genPrintBool,
      genRead(intType, "%d"),
      genRead(charType, "%c"),
      genErr("errOverflow", "fatal error: integer overflow or underflow occurred"),
      genErr("errDivZero", "fatal error: division or modulo by zero"),
      genErr(
        "errBadChar",
        "fatal error: int %d is not ascii character 0-127"
      ) // TODO: fix this so populates %d in err message
    )

    instructions
  }

  private def genFunc(
      func: Func,
      symTable: SymbolTable[Dest],
      allocator: Allocator
  ): ListBuffer[Instruction] = ListBuffer.empty // TODO

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
      case PrintLn(expr) => genPrintStmt(symTable, expr) += CallAsm(Label("_printn"))
      case Read(lval)    => genReadStmt(symTable, lval)
      case Decl(t, ident, value) =>
        // We can allocate the register before we generate rval as the stack machine will
        // only use %eax and %ebx, which are protected
        val dest = allocator.allocateSpace(t)
        symTable.put(ident, dest)
        genDeclStmt(value, dest, symTable)
      case Asgn(lval, value) => genAsgnStmt(lval, value, symTable)
      case _                 => lb() // TODO
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

  private def genRval(value: RVal, symTable: SymbolTable[Dest]): ListBuffer[Instruction] =
    value match {
      case e: Expr => genExpr(e, symTable)
      case a: ArrayLiter => genArray(value.typ.get, a, symTable)
    }

  private def genArray(
   t: Type,
   a: ArrayLiter,
   symTable: SymbolTable[Dest]
  ): ListBuffer[Instruction] = {
    val elemSize = t match {
      case CharType | BoolType => byteSize
      case IntType => intSize
      case StringType | ArrayType(_) | PairType(_, _) => ptrSize
    }
    val size = intSize + a.elems.length * elemSize
    var position = -elemSize
    lb(
      Mov(Immediate(size), Edi()),
      CallAsm(Label("_malloc")),
      Mov(Immediate(a.elems.length), Address(Eax(Size64))),
      AddAsm(Immediate(intSize), Eax(Size64)),
      Push(Eax(Size64)),
      a.elems.flatMap {
        x => position += elemSize
          lb(
            genExpr(x, symTable),
            Pop(Eax(Size64)),
            Pop(Ebx(Size64)),
            Mov(Eax(Size64), Address(Ebx(Size64), Immediate(position))),
            Push(Ebx(Size64))
          )
      },
      Pop(Eax(Size64))
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
          case Some(value) => Mov(value.asInstanceOf[Operand], Eax(Size64))
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
          Je(Label("_errDivZero")),
          // As Cltd will write into edx?? This isn't in reference compiler I just did it.
          Push(Edx(Size64)),
          Cltd,
          Idiv(Ebx(Size32)),
          if (op == Mod) Mov(Edx(Size32), Eax(Size32)) else lb(),
          Movs(Eax(Size32), Eax(Size64)),
          Pop(Edx(Size64)) // Pop back
        )

      case Or | And =>
        lb(
          Cmp(Immediate(1), Eax(Size64)),
          if (op == Or) Je(Label(".L0")) else Jne(Label(".L0")),
          Cmp(Immediate(1), Ebx(Size64)),
          Label(".L0"),
          Set(Eax(Size8), Eq),
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
          Cmovne(Eax(Size64), Esi(Size64)),
          Jne(Label("_errBadChar"))
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
