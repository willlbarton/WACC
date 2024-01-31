package src.main.wacc

object analyser {
  def analyse(program: Program): String = {

    val error = new StringBuilder()
    // Functions may be used before declaration, so we need to do a first pass
    for (f <- program.functions)
      SymbolTable.update(f.name, FuncI(f.t, f.params.map(x => ParamI(x.t))))

    for (f <- program.functions) {
      val st = SymbolTable(SymbolTable)
      f.params.foreach(p => st.update(p.name, ParamI(p.t)))
      error ++= checkFuncStmt(st, f.body)
    }

    error ++= checkMainStmt(SymbolTable, program.body)

    error.toString
  }

  // distinguish between function and main statements since return is not allowed in main
  private def checkFuncStmt(st: SymbolTable, stmt: Stmt): String = stmt match {
    case Skip                 => ""
    case Decl(t, name, value) => checkDecl(st, t, name, value)
    case Asgn(left, value)    => checkAsgn(st, left, value)
    case Read(value)          => checkRead(st, value)
    case Free(expr)           => checkExpr(st, expr)
    case Return(expr)         => checkExpr(st, expr)
    case Exit(expr)           => checkExpr(st, expr)
    case Print(expr)          => checkExpr(st, expr)
    case PrintLn(expr)        => checkExpr(st, expr)
    case IfStmt(cond, body1, body2) =>
      checkExpr(st, cond) ++ checkFuncStmt(st, body1) ++ checkFuncStmt(st, body2)
    case While(cond, body)     => checkExpr(st, cond) ++ checkFuncStmt(st, body)
    case ScopedStmt(stmt)      => checkFuncStmt(SymbolTable(st), stmt)
    case StmtChain(stmt, next) => checkFuncStmt(st, stmt) ++ checkFuncStmt(st, next)
  }

  private def checkMainStmt(st: SymbolTable, stmt: Stmt): String = stmt match {
    case Return(_) => "Return not allowed in main\n"
    case _         => checkFuncStmt(st, stmt)
  }

  private def checkDecl(st: SymbolTable, t: Type, name: Ident, value: RVal): String = {
    val error = new StringBuilder()
    if (st.contains(name)) error ++= s"Variable $name already declared\n"
    else st(name) = VarI(t)
    checkRVal(st, value) match {
      case (msg, Some(t2)) =>
        error ++= msg
        error ++= checkCompatibleTypes(t, t2)
      case (msg, None) => error ++= msg
    }
    error.toString
  }

  private def checkAsgn(st: SymbolTable, left: LVal, value: RVal) = {
    val error = new StringBuilder()
    var t1: Option[Type] = None
    var t2: Option[Type] = None

    checkLVal(st, left) match {
      case (msg, Some(t)) =>
        error ++= msg
        t1 = Some(t)
      case (msg, None) => error ++= msg
    }

    checkRVal(st, value) match {
      case (msg, Some(t)) =>
        error ++= msg
        t2 = Some(t)
      case (msg, None) => error ++= msg
    }

    (t1, t2) match {
      case (Some(t1), Some(t2)) => error ++= checkCompatibleTypes(t1, t2)
      case _                    => ()
    }

    error.toString
  }

  private def checkRead(st: SymbolTable, value: LVal): String = checkLVal(st, value) match {
    case (msg, Some(IntType))  => msg
    case (msg, Some(CharType)) => msg
    case (msg, _)              => msg ++ "Read must take an int or a char\n"
  }

  // checkLVal and checkRVal should also attempt to find the type if possible
  private def checkLVal(st: SymbolTable, value: LVal): (String, Option[Type]) = ???
  private def checkRVal(st: SymbolTable, value: RVal): (String, Option[Type]) = ???
  private def checkExpr(st: SymbolTable, expr: Expr): String = ???
  // etc.
  private def checkCompatibleTypes(t1: Type, t2: Type): String = ???
}
