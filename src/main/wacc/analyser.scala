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
  // just change for your needs TODO: remove
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
    if (symTable.inCurrentScope(ident.name))
      error ++= s"Attempted redeclaration of variable $ident in the same scope\n"
    else
      symTable.put(ident.name, value)

    val (err, typ2) = checkRVal(symTable, value)
    error ++= err
    if (typ2.isDefined && !isCompatibleTypes(typ, typ2.get)) {
      error ++= s"Type mismatch in assignment:\n" +
        s"  Expected ${typ} but got $typ2\n"
    }

    error.toString();
  }

  private def checkAssignment(symTable: SymbolTable, left: LVal, value: RVal): String = {
    val error = new StringBuilder()
    var typ1: Option[Type] = None
    checkLVal(symTable, left) match {
      case Left(err) => error ++= err
      case Right(typ) => typ1 = Some(typ)
    }
    val (err, typ2) = checkRVal(symTable, value)
    error ++= err
    if (typ1.isDefined && typ2.isDefined && !isCompatibleTypes(typ1.get, typ2.get)) {
      error ++= s"Type mismatch in assignment:\n" +
        s"  Expected ${typ1.get} but got $typ2\n"
    }
    error.toString()
  }

  private def checkExpr(symTable: SymbolTable, expr: Expr): (String, Option[Type]) = expr match {
    case Integer(_)    => ("" , Some(IntType))
    case Bool(_)       => ("" , Some(BoolType))
    case Character(_)  => ("" , Some(CharType))
    case StringAtom(_) => ("" , Some(StringType))
    case Ident(name)   => checkIdent(symTable, name) match {
      case Left(err) => (err, None)
      case Right(typ) => ("", Some(typ))
    }
    case ArrayElem(ident, exprs) => checkArrayElem(symTable, ident, exprs)
    case BracketedExpr(expr)     => checkExpr(symTable, expr)
    case Null                    => ???
    case UnaryApp(op, expr) => checkUnaryApp(symTable, op, expr)
    case BinaryApp(op, left, right) => checkBinaryApp(symTable, op, left, right)
  }

  private def checkIdent(symTable: SymbolTable, name: String): Either[String, Type] =
    symTable(name) match {
      case None => Left(s"Variable $name used before declaration!\n")
      case Some(obj) =>
        assert(obj.typ.isDefined) // Everything in symbol table should have a type
        Right(obj.typ.get)
    }

  private def checkUnaryApp(symTable: SymbolTable, op: UnaryOp, expr: Expr): (String, Option[Type]) = {
    val error = new StringBuilder()
    val (err, typ) = checkExpr(symTable, expr)
    error ++= err

    var retType: Option[Type] = None
    if (typ.isDefined) {
      val someType = typ.get
      op match {
        case Chr =>
          if (someType == IntType) retType = Some(CharType)
          else error ++= s"Expected int type for '$op' operator\n"
        case Len =>
          if (someType == StringType || someType.isInstanceOf[ArrayType]) retType = Some(IntType)
          else error ++= s"Expected string or array type for '$op' operator\n"
        case Neg =>
          if (someType == IntType) retType = Some(IntType)
          else error ++= s"Expected int type for '$op' operator\n"
        case Not =>
          if (someType == BoolType) retType = Some(BoolType)
          else error ++= s"Expected bool type for '$op' operator\n"
        case Ord =>
          if (someType == CharType) retType = Some(IntType)
          else error ++= s"Expected char type for '$op' operator\n"
      }
    }
    (error.toString, retType)
  }

  private def checkBinaryApp(symTable: SymbolTable, op: BinaryOp, left: Expr, right: Expr): (String, Option[Type]) = {
    val error = new StringBuilder()
    val (err1, typ1) = checkExpr(symTable, left)
    error ++= err1
    val (err2, typ2) = checkExpr(symTable, right)
    error ++= err2
    var retType: Option[Type] = None
    if (typ1.isDefined && typ2.isDefined) {
      val someType1 = typ1.get
      val someType2 = typ2.get
      op match {
        case And | Or =>
          if (someType1 == BoolType && someType2 == BoolType) retType = Some(BoolType)
          else error ++= s"Expected bool type in application of '$op' operator\n"
        case Eq | NotEq =>
          if (isCompatibleTypes(someType1, someType2)) retType = Some(BoolType)
          else error ++= s"Expected compatible types in application of '$op' operator\n"
        case Gt | GtEq | Lt | LtEq =>
          if (someType1 == IntType && someType2 == IntType) retType = Some(BoolType)
          else error ++= s"Expected int type in application of '$op' operator\n"
        case Add =>
          if (someType1 == IntType && someType2 == IntType) {
            // TODO: check for overflow
            retType = Some(IntType)
          } else if (someType1 == StringType && someType2 == StringType) retType = Some(StringType)
          else error ++= s"Expected int or string type in application of '$op' operator\n"
        case Sub | Mul | Div | Mod =>
          if (someType1 == IntType && someType2 == IntType) {
            // TODO: check for overflow
            retType = Some(IntType)
          } else error ++= s"Expected int type in application of '$op' operator\n"
      }
    }
    (error.toString, retType)
  }

  private def checkArrayElem(symTable: SymbolTable, ident: Ident, exprs: List[Expr]) = ???

  private def checkPairElem(symTable: SymbolTable, pairElem: PairElemType) = ???

  private def checkLVal(symTable: SymbolTable, lval: LVal): Either[String, Type] = lval match {
    case Ident(name) => checkIdent(symTable, name)
    case ArrayElem(ident, exprs) => checkArrayElem(symTable, ident, exprs)
    case Fst(value)              => checkLVal(symTable, value)
    case Snd(value)              => checkLVal(symTable, value)
  }

  private def checkRVal(symTable: SymbolTable, value: RVal): (String, Option[Type]) = value match {
    case ArrayLiter(exprs)     => ???
    case NewPair(expr1, expr2) => ???
    case Fst(_) | Snd(_)       => checkLVal(symTable, value.asInstanceOf[LVal]) match {
      case Left(err) => (err, None) // couldn't infer type
      case Right(typ) => ("", Some(typ))
    }
    case Call(ident, exprs) =>
      symTable(ident.name) match {
        case None => (s"Usage of undeclared function: $ident!\n", None)
        case Some(fun) =>
          assert(fun.typ.isDefined) // Everything in symbol table should have a type
          ("", fun.typ)
      }
    case _ => checkExpr(symTable, value.asInstanceOf[Expr])
  }

  private def isCompatibleTypes(typ1: Type, typ2: Type): Boolean =
    typ1 == typ2 || typ1 == StringType && typ2 == ArrayType(
      CharType
    ) || typ2 == StringType && typ1 == ArrayType(CharType)

}
