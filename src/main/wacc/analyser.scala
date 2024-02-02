package src.main.wacc

// object analyser {
//   def analyse(program: Program): String = {
//     val error = new StringBuilder()
//     val STop = SymbolTable(None);

//     // Functions may be used before declaration, so we need to do a first pass
//     // Add all functions to STop
//     for (f <- program.functions)
//       STop.update(f.name, FuncI(f.t, f.params.map(x => ParamI(x.t))))

//     // Adds child symbol table to function object
//     for (f <- program.functions) {
//       val st = STop.makeChild
//       f.params.foreach(p => st.update(p.name, ParamI(p.t)))
//       error ++= checkFuncStmt(st, f.body)
//     }

//     error ++= checkMainStmt(STop, program.body)

//     error.toString
//   }

//   // distinguish between function and main statements since return is not allowed in main
//   private def checkFuncStmt(st: SymbolTable, stmt: Stmt): String = stmt match {
//     case Return(expr)         => checkExpr(st, expr)
//     case IfStmt(cond, body1, body2) =>
//       checkExpr(st, cond) ++ checkFuncStmt(st, body1) ++ checkFuncStmt(st, body2)
//     case While(cond, body)     => checkExpr(st, cond) ++ checkFuncStmt(st, body)
//     case ScopedStmt(stmt)      => checkFuncStmt(st.makeChild, stmt)
//     case StmtChain(stmt, next) => checkFuncStmt(st, stmt) ++ checkFuncStmt(st, next)
//     case _                     => checkLeafStatement(st, stmt)
//   }

//   private def checkMainStmt(st: SymbolTable, stmt: Stmt): String = stmt match {
//     case Return(_) => "Return not allowed in main\n"
//     case IfStmt(cond, body1, body2) =>
//       checkExpr(st, cond) ++ checkMainStmt(st, body1) ++ checkMainStmt(st, body2)
//     case While(cond, body) => checkExpr(st, cond) ++ checkMainStmt(st, body)
//     case ScopedStmt(stmt) => checkMainStmt(st.makeChild, stmt)
//     case StmtChain(stmt, next) => checkMainStmt(st, stmt) ++ checkMainStmt(st, next)
//     case _ => checkLeafStatement(st, stmt)
//   }

//   private def checkLeafStatement(st: SymbolTable, stmt: Stmt): String = stmt match {
//     case Skip                 => ""
//     case Decl(t, name, value) => checkDecl(st, t, name, value)
//     case Asgn(left, value)    => checkAsgn(st, left, value)
//     case Read(value)          => checkRead(st, value)
//     case Free(expr)           => checkExpr(st, expr)
//     case Exit(expr)           => checkExpr(st, expr)
//     case Print(expr)          => checkExpr(st, expr)
//     case PrintLn(expr)        => checkExpr(st, expr)
//   }

//   private def checkDecl(st: SymbolTable, t: Type, name: Ident, value: RVal): String = {
//     val error = new StringBuilder()
//     if (st.contains(name)) error ++= s"Variable $name already declared\n"
//     else st(name) = VarI(t)
//     checkRVal(st, value) match {
//       case (msg, Some(t2)) =>
//         error ++= msg
//         error ++= checkCompatibleTypes(t, t2)
//       case (msg, None) => error ++= msg
//     }
//     error.toString
//   }

//   private def checkAsgn(st: SymbolTable, left: LVal, value: RVal) = {
//     val error = new StringBuilder()
//     var t1: Option[Type] = None
//     var t2: Option[Type] = None

//     checkLVal(st, left) match {
//       case (msg, Some(t)) =>
//         error ++= msg
//         t1 = Some(t)
//       case (msg, None) => error ++= msg
//     }

//     checkRVal(st, value) match {
//       case (msg, Some(t)) =>
//         error ++= msg
//         t2 = Some(t)
//       case (msg, None) => error ++= msg
//     }

//     (t1, t2) match {
//       case (Some(t1), Some(t2)) => error ++= checkCompatibleTypes(t1, t2)
//       case _                    => ()
//     }

//     error.toString
//   }

//   private def checkRead(st: SymbolTable, value: LVal): String = checkLVal(st, value) match {
//     case (msg, Some(IntType))  => msg
//     case (msg, Some(CharType)) => msg
//     case (msg, _)              => msg ++ "Read must take an int or a char\n"
//   }

//   // checkLVal and checkRVal should also attempt to find the type if possible
//   private def checkLVal(st: SymbolTable, value: LVal): (String, Option[Type]) = ???
//   private def checkRVal(st: SymbolTable, value: RVal): (String, Option[Type]) = ???
//   private def checkExpr(st: SymbolTable, expr: Expr): String = ???
//   // etc.
//   private def checkCompatibleTypes(t1: Type, t2: Type): String = ???
// }

object analyser {

  def analyse(program: Program): String = {
    val error = new StringBuilder()
    val mainSymTable = SymbolTable(None);

    // Functions may be used before declaration, so we need to do a first pass
    // Add all functions to STop
    for (f <- program.functions)
      mainSymTable.put(f.ident.name, f)

    // Adds child symbol table to function object
    for (f <- program.functions) {
      val symTable = mainSymTable.makeChild
      f.params.foreach(p => symTable.put(f.ident.name, f))
      // error ++= checkFuncStmt(st, f.body)
    }

    // error ++= checkMainStmt(STop, program.body)

    error.toString
  }

  // use for testing a check function ########################
  // just change for your needs
  def main(args: Array[String]) = {
    val mainSymTable = SymbolTable(None);
    println(checkExpr(mainSymTable, BinaryApp(Add, Integer(1), Integer(1))))
  }

  private def handleDeclaration(
      symTable: SymbolTable,
      typ: Type,
      ident: Ident,
      value: RVal
  ): String = {
    val error = new StringBuilder()
    if (symTable.inCurrentScope(ident.name)) {
      error ++= s"Variable $ident already declared in scope\n"
    } else {
      symTable.put(
        ident.name, {
          typ match { // IDK WHAT TO SET THESE VALS - make them optional ????
            case IntType    => Integer(69420)
            case BoolType   => Bool(false)
            case CharType   => Character('#')
            case StringType => StringAtom("WACC")
            case ArrayType(t) =>
              ???
            case PairType(fst, snd) =>
              ???
            case Pair => ???
          }
        }
      )
    }

    error ++= {
      checkRVal(symTable, value) match {
        case Left(err) => err
        case Right(typ2) =>
          if (typ == typ2) ""
          else s"Type mismatch: $typ and $typ2\n"
      }
    }

    error.toString();
  }

  private def checkAssignment(symTable: SymbolTable, left: LVal, value: RVal): String = {
    checkLVal(symTable, left) match {
      case Left(err) => err
      case Right(typ1) =>
        checkRVal(symTable, value) match {
          case Left(err) => err
          case Right(typ2) =>
            if (typ1 == typ2) ""
            else s"Type mismatch: $typ1 and $typ2\n"
        }
    }
  }

  private def checkExpr(symTable: SymbolTable, expr: Expr): Either[String, Type] = expr match {
    case Integer(_)    => Right(IntType)
    case Bool(_)       => Right(BoolType)
    case Character(_)  => Right(CharType)
    case StringAtom(_) => Right(StringType)
    case Ident(name)   => checkIdent(symTable, name)
    case ArrayElem(ident, exprs) => checkArrayElem(symTable, ident, exprs)
    case BracketedExpr(expr)     => checkExpr(symTable, expr)
    case Null                    => ???
    case UnaryApp(op, expr) => checkUnaryApp(symTable, op, expr)
    case BinaryApp(op, left, right) => checkBinaryApp(symTable, op, left, right)

  }

  private def checkIdent(symTable: SymbolTable, name: String) =
    symTable(name) match {
      case None => Left(s"Variable $name not declared\n")
      case Some(obj) =>
        obj.typ match {
          case None => Left(s"Variable type of $name not declared\n")
          case Some(typ) => Right(typ)
        }
    }

  private def checkUnaryApp(symTable: SymbolTable, op: UnaryOp, expr: Expr) =
    checkExpr(symTable, expr) match {
      case Left(err) => Left(err)
      case Right(typ) =>
        op match {
          case Chr =>
            if (typ == IntType) Right(IntType)
            else Left(s"Expected int type for '$op' operator ")
          case Len => ???
          case Neg =>
            if (typ == IntType) Right(IntType)
            else Left(s"Expected int type for '$op' operator")
          case Not =>
            if (typ == BoolType) Right(BoolType)
            else Left(s"Expected bool type for '$op' operator")
          case Ord =>
            if (typ == CharType) Right(CharType)
            else Left(s"Expected char type for '$op' operator")
        }
    }

  private def checkBinaryApp(symTable: SymbolTable, op: BinaryOp, left: Expr, right: Expr) =
    checkExpr(symTable, left) match {
      case Left(err) => Left(err)
      case Right(typ1) =>
        checkExpr(symTable, right) match {
          case Left(err) => Left(err)
          case Right(typ2) =>
            op match {
              case And | Or =>
                if (typ1 == BoolType && typ2 == BoolType) Right(BoolType)
                else Left(s"Expected bool type for '$op' operator\n")
              case Eq | NotEq =>
                if (isCompatibleTypes(typ1, typ2)) Right(BoolType)
                else Left(s"Expected compatible types for '$op' operator\n")
              case Gt | GtEq | Lt | LtEq =>
                if (typ1 == IntType && typ2 == IntType) Right(BoolType)
                else Left(s"Expected int type for '$op' operator\n")
              case Add =>
                if (typ1 == IntType && typ2 == IntType) Right(IntType)
                else if (typ1 == StringType && typ2 == StringType) Right(StringType)
                else Left(s"Expected int or string type for '$op' operator\n")
              case Sub | Mul | Div | Mod =>
                if (typ1 == IntType && typ2 == IntType) Right(IntType)
                else Left(s"Expected int type for '$op' operator\n")
            }
        }
    }

  private def checkArrayElem(symTable: SymbolTable, ident: Ident, exprs: List[Expr]) = ???

  private def checkPairElem(symTable: SymbolTable, pairElem: PairElemType) = ???

  private def checkLVal(symTable: SymbolTable, lval: LVal): Either[String, Type] = lval match {
    case Ident(name) => checkIdent(symTable, name)
    case ArrayElem(ident, exprs) => checkArrayElem(symTable, ident, exprs)
    case Fst(value)              => checkLVal(symTable, value)
    case Snd(value)              => checkLVal(symTable, value)
  }

  private def checkRVal(symTable: SymbolTable, value: RVal): Either[String, Type] = value match {
    case ArrayLiter(exprs)     => ???
    case NewPair(expr1, expr2) => ???
    case Fst(_) | Snd(_)       => checkLVal(symTable, value.asInstanceOf[LVal])
    case Call(ident, exprs) =>
      symTable(ident.name) match {
        case None => Left(s"Function $ident not declared\n")
        case Some(fun) =>
          fun.typ match {
            case None      => Left(s"Function type of $ident not declared\n")
            case Some(typ) => Right(typ)
          }
      }
    case _ => checkExpr(symTable, value.asInstanceOf[Expr])
  }

  private def isCompatibleTypes(typ1: Type, typ2: Type): Boolean =
    typ1 == typ2 || typ1 == StringType && typ2 == ArrayType(
      CharType
    ) || typ2 == StringType && typ1 == ArrayType(CharType)

}
