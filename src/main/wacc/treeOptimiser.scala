package src.main.wacc

object treeOptimiser {
  def optimise(tree: Program): Program = {
    val Program(funcs, body) = tree
    val p = Program(funcs.map(optimiseFunc), body.map(optimiseStmt))
    p.vars = tree.vars
    p
  }

  private def optimiseFunc(func: Func): Func = {
    val Func(t, ident, params, body) = func
    val f = Func(t, ident, params, body.map(optimiseStmt))
    f.vars = func.vars
    f
  }

  private def optimiseStmt(stmt: Stmt): Stmt = stmt match {
    case Decl(t, ident, value) => Decl(t, ident, optimiseRVal(value))
    case Asgn(left, value) => Asgn(optimiseLVal(left), optimiseRVal(value))
    case Read(value) => Read(optimiseLVal(value))
    case Free(expr) => Free(optimiseExpr(expr))
    case Return(expr) => Return(optimiseExpr(expr))
    case Exit(expr) => Exit(optimiseExpr(expr))
    case Print(expr) => Print(optimiseExpr(expr))
    case PrintLn(expr) => PrintLn(optimiseExpr(expr))
    case f1@IfStmt(cond, body1, body2) =>
      val f2 = IfStmt(optimiseExpr(cond), body1.map(optimiseStmt), body2.map(optimiseStmt))
      f2.branch1Vars = f1.branch1Vars
      f2.branch2Vars = f1.branch2Vars
      f2
    case w1@While(cond, body) =>
      val w2 = While(optimiseExpr(cond), body.map(optimiseStmt))
      w2.vars = w1.vars
      w2
    case s@ScopedStmt(stmts) =>
      val s2 = ScopedStmt(stmts.map(optimiseStmt))
      s2.vars = s.vars
      s2
    case s => s
  }

  private def optimiseLVal(value: LVal): LVal = optimiseRVal(value).asInstanceOf[LVal]

  private def optimiseRVal(value: RVal): RVal = {
    val v = value match {
      case expr: Expr => optimiseExpr(expr)
      case ArrayLiter(elems) => ArrayLiter(elems.map(optimiseExpr))
      case NewPair(fst, snd) => NewPair(optimiseExpr(fst), optimiseExpr(snd))
      case Call(ident, args) => Call(ident, args.map(optimiseExpr))
      case r => r
    }
    v.typ = value.typ
    v
  }

  private def optimiseExpr(value: Expr): Expr = {
    val v = value match {
      case UnaryApp(op, expr) => optimiseUnaryApp(op, expr)
      case BinaryApp(op, left, right) => optimiseBinaryApp(op, left, right)
      case ArrayElem(ident, exprs) => ArrayElem(ident, exprs.map(optimiseExpr))
      case BracketedExpr(expr) => optimiseBracketedExpr(expr)
      case e => e
    }
    v.typ = value.typ
    v
  }

  private def optimiseBracketedExpr(expr: Expr): Expr =
    analyser.evalConst(BracketedExpr(expr)) match {
      case Some(value) => Integer(value.toInt)
      case None => BracketedExpr(optimiseExpr(expr))
    }
  private def optimiseBinaryApp(op: BinaryOp, left: Expr, right: Expr): Expr =
    analyser.evalConst(BinaryApp(op, left, right)) match {
      case Some(value) => Integer(value.toInt)
      case None => BinaryApp(op, optimiseExpr(left), optimiseExpr(right))
    }
  private def optimiseUnaryApp(op: UnaryOp, expr: Expr): Expr =
    analyser.evalConst(UnaryApp(op, expr)) match {
      case Some(value) => Integer(value.toInt)
      case None => UnaryApp(op, optimiseExpr(expr))
    }
}