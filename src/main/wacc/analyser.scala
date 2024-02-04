package src.main.wacc

object analyser {

  def analyse(program: Program): String = {
    val error = new StringBuilder()
    val mainSymTable = SymbolTable(None)

    // Functions may be used before declaration, so we need to do a first pass
    for (f <- program.functions)
      mainSymTable.put(f.ident.name, f)

    for (f <- program.functions) {
      val symTable = mainSymTable.makeChild
      f.params.foreach(p => if (symTable.inCurrentScope(p.ident.name))
          error ++= s"Attempted redeclaration of parameter '${p.ident}'\n" +
            s"  in function '${f.ident}'(${f.params.mkString(", ")})\n"
        else {
          p.ident.typ = Some(p.t)
          symTable.put(p.ident.name, p.ident)
        })
       error ++= checkFuncStmt(symTable, f.body, f.t)
    }

    error ++= checkMainStmt(mainSymTable, program.body)

    error.toString
  }

  // distinguish between function and main statements since return is not allowed in main
  private def checkFuncStmt(st: SymbolTable, stmt: Stmt, typ: Type): String = stmt match {
    case Return(expr) =>
      val (err, expType) = checkExpr(st, expr)
      err ++ (if (expType.isDefined && !isWeakerType(typ, expType.get)) {
        typeErrorMsg("function return", s"return $expr", s"$typ", s"${expType.get}")
      } else "")
    case IfStmt(cond, body1, body2) =>
      checkCond(st, cond, isIf = true) ++
        checkFuncStmt(st, body1, typ) ++ checkFuncStmt(st, body2, typ)
    case While(cond, body)     => checkCond(st, cond, isIf = false) ++ checkFuncStmt(st, body, typ)
    case ScopedStmt(stmt)      => checkFuncStmt(st.makeChild, stmt, typ)
    case StmtChain(stmt, next) => checkFuncStmt(st, stmt, typ) ++ checkFuncStmt(st, next, typ)
    case _                     => checkLeafStatement(st, stmt)
  }

  private def checkMainStmt(st: SymbolTable, stmt: Stmt): String = stmt match {
    case Return(_) => "Return not allowed in main\n"
    case IfStmt(cond, body1, body2) =>
      checkCond(st, cond, isIf = true) ++ checkMainStmt(st, body1) ++ checkMainStmt(st, body2)
    case While(cond, body) => checkCond(st, cond, isIf = false) ++ checkMainStmt(st, body)
    case ScopedStmt(stmt) => checkMainStmt(st.makeChild, stmt)
    case StmtChain(stmt, next) => checkMainStmt(st, stmt) ++ checkMainStmt(st, next)
    case _ => checkLeafStatement(st, stmt)
  }

  private def checkCond(st: SymbolTable, cond: Expr, isIf: Boolean) = {
    val (err, typ) = checkExpr(st, cond)
    err ++ (if (typ.isDefined && typ.get != BoolType)
      typeErrorMsg((if (isIf) "if statement" else "loop") + " conditional",
        if (isIf) s"if $cond then" else s"while $cond do", "bool", s"${typ.get}")
    else "")
  }

  private def checkLeafStatement(st: SymbolTable, stmt: Stmt): String = stmt match {
     case Skip                 => ""
     case Decl(t, name, value) => handleDeclaration(st, t, name, value)
     case Asgn(left, value)    => checkAssignment(st, left, value)
     case Read(value)          => checkRead(st, value)
     case Free(expr)           => checkExpr(st, expr) match {
       case (err, Some(PairType(_, _))) => err
       case (err, Some(ArrayType(_)))   => err
       case (err, None)                 => err
       case (err, Some(typ)) =>
         err ++ typeErrorMsg("free statement", s"free $expr", "pair' or 'array", s"$typ")
     }
     case Exit(expr)           => checkExpr(st, expr) match {
       case (err, Some(t)) if t != IntType =>
         err ++ typeErrorMsg("exit statement", s"exit $expr", "int", s"$t")
       case (err, _) => err
     }
     case Print(expr)          => checkExpr(st, expr)._1
     case PrintLn(expr)        => checkExpr(st, expr)._1
     case _ => throw new IllegalArgumentException("Non-leaf statement in checkLeafStatement\n")
   }

  private def handleDeclaration(
      symTable: SymbolTable,
      typ: Type,
      ident: Ident,
      value: RVal
  ): String = {
    val error = new StringBuilder()

    val (err, typ2) = checkRVal(symTable, value)
    error ++= err
    if (typ2.isDefined && !isWeakerType(typ, typ2.get)) {
      error ++= typeErrorMsg(
        s"declaration of variable $ident", s"$typ $ident = $value", s"$typ", s"${typ2.get}")
    }

    if (symTable.inCurrentScope(ident.name))
      error ++= s"Attempted redeclaration of variable '$ident' in same scope\n"
    else
      value match {
        case Ident(name) => checkIdent(symTable, name) match {
          case Left(err) => error ++= err
          case Right(typ2) =>
            if (isWeakerType(typ, typ2)) symTable.put(ident.name, symTable(name).get)
        }
        case _ => symTable.put(ident.name, value)
      }

    value.typ = typ2
    error.toString
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
    if (typ1.isDefined && typ2.isDefined && !isWeakerType(typ1.get, typ2.get))
      error ++= typeErrorMsg("assignment", s"$left = $value", s"$typ1", s"$typ2")
    error.toString
  }

  private def checkRead(st: SymbolTable, value: LVal): String = checkLVal(st, value) match {
    case Left(err) => err
    case Right(typ) => typ match {
      case IntType  => ""
      case CharType => ""
      case _        =>
        typeErrorMsg("read statement", s"read $value", "int' or 'char", s"$typ")
    }
  }

  @scala.annotation.tailrec
  private def checkExpr(symTable: SymbolTable, expr: Expr): (String, Option[Type]) = expr match {
    case Integer(_)    => ("" , Some(IntType))
    case Bool(_)       => ("" , Some(BoolType))
    case Character(_)  => ("" , Some(CharType))
    case StringAtom(_) => ("" , Some(StringType))
    case Ident(name)   => checkIdent(symTable, name) match {
      case Left(err) => (err, None)
      case Right(typ) => ("", Some(typ))
    }
    case ArrayElem(ident, exprs) => checkArrayElem(symTable, ident, exprs) match {
      case Left(err) => (err, None)
      case Right(typ) => ("", Some(typ))
    }
    case BracketedExpr(expr)     => checkExpr(symTable, expr)
    case Null                    => ("", Some(Pair))
    case UnaryApp(op, expr) => checkUnaryApp(symTable, op, expr)
    case BinaryApp(op, left, right) => checkBinaryApp(symTable, op, left, right)
  }

  private def checkIdent(symTable: SymbolTable, name: String): Either[String, Type] =
    symTable(name) match {
      case None => Left(s"Variable '$name' used before declaration!\n")
      case Some(obj) =>
        assert(obj.typ.isDefined) // Everything in symbol table should have a type
        Right(obj.typ.get)
    }

  private def evalConst(expr: Expr): Option[BigInt] = expr match {
    case Integer(i) => Some(i)
    case UnaryApp(Neg, e) => evalConst(e).map(-_)
    case UnaryApp(Ord, Character(c)) => Some(c.toInt)
    case UnaryApp(Ord, UnaryApp(Chr, e)) => evalConst(e)
    case BinaryApp(op, e1, e2) =>
      val val1 = evalConst(e1)
      val val2 = evalConst(e2)
      if (val1.isDefined && val2.isDefined) {
        val v1 = val1.get
        val v2 = val2.get
        op match {
          case Add => Some(v1 + v2)
          case Sub => Some(v1 - v2)
          case Mul => Some(v1 * v2)
          case Div => if (v2 == 0) None else Some(v1 / v2)
          case Mod => Some(v1 % v2)
          case _ => None
        }
      } else None
    case _ => None
  }

  private def checkConstantApplication(left: Expr, right: Expr, op: BinaryOp): String = {
    val leftv = evalConst(left)
    val rightv = evalConst(right)
    if (leftv.isDefined && rightv.isDefined) {
      val lv = leftv.get
      val rv = rightv.get
      op match {
        case Add => if (lv + rv > Int.MaxValue || lv + rv < Int.MinValue)
          s"Overflow error in expression: $left + $right:\n" +
            "  Addition of int literals would result in overflow\n" else ""
        case Sub => if (lv - rv < Int.MinValue || lv - rv > Int.MaxValue) {
          s"Overflow error in expression: $left - $right:\n" +
          "  Subtraction of int literals would result in underflow\n"
        } else ""
        case Mul => if (lv * rv > Int.MaxValue || lv * rv < Int.MinValue)
          s"Overflow error in expression: $left * $right:\n" +
          "  Multiplication of int literals would result in overflow\n" else ""
        case Div => if (rv == 0) s"Division by zero error in expression: $left / $right\n" else ""
        case _ => ""
      }
    } else ""
  }

  private def unaryAppErrMsg(op: UnaryOp, typ: Type, expr: Expr): String = {
    val expected: String = op match {
      case Chr | Neg => IntType.toString
      case Ord => CharType.toString
      case Len => s"$StringType or array"
      case Not => BoolType.toString
    }
    typeErrorMsg(s"application of $op operator",
      s"expression: $expr",
      expected,
      s"$typ")
  }

  private def checkUnaryApp(
    symTable: SymbolTable, op: UnaryOp, expr: Expr
  ): (String, Option[Type]) = {
    val error = new StringBuilder()
    val (err, typ) = checkExpr(symTable, expr)
    error ++= err

    var retType: Option[Type] = None
    if (typ.isDefined) {
      val someType = typ.get
      op match {
        case Chr =>
          if (someType == IntType) retType = Some(CharType)
          else error ++= unaryAppErrMsg(Chr, someType, expr)
        case Len =>
          if (someType == StringType || someType.isInstanceOf[ArrayType]) retType = Some(IntType)
          else error ++= unaryAppErrMsg(Len, someType, expr)
        case Neg =>
          if (someType == IntType) {
            if (evalConst(expr).contains(Int.MinValue))
              error ++= "Negation of int literal would result in overflow\n" +
                s"  in expression: $expr\n"
            retType = Some(IntType)
          }
          else error ++= unaryAppErrMsg(Neg, someType, expr)
        case Not =>
          if (someType == BoolType) retType = Some(BoolType)
          else error ++= unaryAppErrMsg(Not, someType, expr)
        case Ord =>
          if (someType == CharType) retType = Some(IntType)
          else error ++= unaryAppErrMsg(Ord, someType, expr)
      }
    }
    (error.toString, retType)
  }

  private def binaryAppErrMsg(op: BinaryOp, typ1: Type, typ2: Type, expr: Expr): String = {
    val expected: String = op match {
      case And | Or => BoolType.toString
      case Eq | NotEq => "compatible types"
      case Add => s"$IntType' or '$StringType"
      case Gt | GtEq | Lt | LtEq | Sub | Mul | Div | Mod => IntType.toString
    }
    typeErrorMsg(s"application of '$op' operator",
      s"expression: $expr",
      expected,
      s"$typ1' and '$typ2")
  }

  private def checkBinaryApp(
    symTable: SymbolTable, op: BinaryOp, left: Expr, right: Expr
  ): (String, Option[Type]) = {
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
          else error ++= binaryAppErrMsg(op, someType1, someType2, BinaryApp(op, left, right))
        case Eq | NotEq =>
          if (isCompatibleTypes(someType1, someType2)) retType = Some(BoolType)
          else error ++= binaryAppErrMsg(op, someType1, someType2, BinaryApp(op, left, right))
        case Gt | GtEq | Lt | LtEq =>
          if (someType1 == IntType && someType2 == IntType ||
            someType1 == CharType && someType2 == CharType) retType = Some(BoolType)
          else error ++= binaryAppErrMsg(op, someType1, someType2, BinaryApp(op, left, right))
        case Add =>
          if (someType1 == IntType && someType2 == IntType) {
            error ++= checkConstantApplication(left, right, Add)
            retType = Some(IntType)
          } else if (someType1 == StringType && someType2 == StringType) retType = Some(StringType)
          else error ++= binaryAppErrMsg(op, someType1, someType2, BinaryApp(op, left, right))
        case Sub | Mul | Div | Mod =>
          if (someType1 == IntType && someType2 == IntType) {
            error ++= checkConstantApplication(left, right, op)
            retType = Some(IntType)
          } else error ++= binaryAppErrMsg(op, someType1, someType2, BinaryApp(op, left, right))
      }
    }
    (error.toString, retType)
  }

  private def checkArrayElem(
    symTable: SymbolTable, ident: Ident, exprs: List[Expr]
  ): Either[String, Type] = {
    val error = new StringBuilder()
    var typ: Option[Type] = None
    checkIdent(symTable, ident.name) match {
      case Left(err) => error ++= err
      case Right(ArrayType(typ2)) => typ = Some(typ2)
      case Right(typ2) => error ++= typeErrorMsg(
        "array access", s"$ident[${exprs.mkString("][")}]", "array", s"$typ2")
    }
    exprs.foreach(expr => {
      val (err, typ2) = checkExpr(symTable, expr)
      error ++= err
      if (typ2.isDefined && typ2.get != IntType)
        error ++= typeErrorMsg(
          "array index", s"$ident[${exprs.mkString("][")}]", "int", s"${typ2.get}")
    })
    if (error.isEmpty) Right(typ.get) else Left(error.toString)
  }

  private def checkLVal(symTable: SymbolTable, lval: LVal): Either[String, Type] = lval match {
    case Ident(name) => checkIdent(symTable, name)
    case ArrayElem(ident, exprs) => checkArrayElem(symTable, ident, exprs)
    case Fst(value)              => checkLVal(symTable, value) match {
      case Left(err) => Left(err)
      case Right(PairType(typ, _)) => Right(typ)
      case Right(typ) => Left(typeErrorMsg(
        "pair element access", s"fst $value", "pair", s"$typ"))
    }
    case Snd(value)              => checkLVal(symTable, value) match {
      case Left(err) => Left(err)
      case Right(PairType(_, typ)) => Right(typ)
      case Right(typ) => Left(typeErrorMsg(
        "pair element access", s"snd $value", "pair", s"$typ"))
    }
  }

  private def checkRVal(symTable: SymbolTable, value: RVal): (String, Option[Type]) = value match {
    case ArrayLiter(exprs)     => checkArrayLiteral(symTable, exprs)
    case NewPair(expr1, expr2) => checkNewPair(symTable, expr1, expr2)
    case Fst(_) | Snd(_)       => checkLVal(symTable, value.asInstanceOf[LVal]) match {
      case Left(err) => (err, None) // couldn't infer type
      case Right(typ) => ("", Some(typ))
    }
    case Call(ident, exprs) => checkCall(symTable, ident, exprs)
    case _ => checkExpr(symTable, value.asInstanceOf[Expr])
  }

  private def checkNewPair(symTable: SymbolTable, expr1: Expr, expr2: Expr) = {
    val (err1, typ1) = checkExpr(symTable, expr1)
    val (err2, typ2) = checkExpr(symTable, expr2)
    val err = err1 ++ err2
    if (typ1.isDefined && typ2.isDefined) {
      val ltype = if (!typ1.get.isInstanceOf[PairElemType]) Pair
                  else typ1.get.asInstanceOf[PairElemType]
      val rtype = if (!typ2.get.isInstanceOf[PairElemType]) Pair
                  else typ2.get.asInstanceOf[PairElemType]
      (err, Some(PairType(ltype, rtype)))
    } else (err, None)
  }

  private def checkArrayLiteral(symTable: SymbolTable, exprs: List[Expr]): (String, Option[Type]) = {
    val errors = new StringBuilder()
    val errTyps = exprs.map(expr => checkExpr(symTable, expr))
    for ((err, _) <- errTyps) errors ++= err
    val typs = errTyps.map(_._2)
    if (typs.nonEmpty && typs.forall(_.isDefined)) {
      var typ = typs.head.get
      for (t <- typs.tail) {
        if (!isWeakerType(typ, t.get)) {
          if (isWeakerType(t.get, typ)) {
            typ = t.get // weaken type
          } else {
            errors ++= s"Non-compatible types in array literal\n" +
              s"  in [${exprs.mkString(", ")}]\n"
            return (errors.toString, None)
          }
        }
      }
      (errors.toString, Some(ArrayType(typ)))
    } else (errors.toString, None)
  }

  private def checkCall(symTable: SymbolTable, ident: Ident, exprs: List[Expr]) = {
    symTable(ident.name) match {
      case None => (s"Usage of undeclared function: $ident!\n", None)
      case Some(fun) => fun match {
        case Func(typ, _, params, _) =>
          val errors = new StringBuilder()
          if (params.length != exprs.length)
            errors ++=
              s"Incorrect number of arguments in:\n  call $ident(${exprs.mkString(", ")})\n"
          for ((param, expr) <- params.zip(exprs)) {
            val (err, ptype) = checkExpr(symTable, expr)
            errors ++= err
            if (ptype.isDefined) {
              if (!isWeakerType(param.t, ptype.get))
                errors ++= typeErrorMsg(
                  s"function argument ${param.ident.name}",
                  s"$ident(${exprs.mkString(", ")})",
                  s"${param.t}", s"${ptype.get}")
              else symTable.put(param.ident.name, expr)
            }
          }
          (errors.toString, Some(typ))

        case obj => (typeErrorMsg("Function call",
          s"call $ident(${exprs.mkString(", ")})", "function", s"${obj.typ.get}"),
          None)
      }
    }
  }

  private def isWeakerType(weaker: Type, stronger: Type): Boolean = {
    weaker == stronger ||
      (weaker == Pair && stronger.isInstanceOf[PairType]) ||
      (stronger == Pair && weaker.isInstanceOf[PairType]) ||
      weaker == StringType && stronger == ArrayType(CharType) ||
      ((weaker, stronger) match {
      case (ArrayType(t1), ArrayType(t2)) => isWeakerType(t1, t2)
      case (PairType(f1, s1), PairType(f2, s2)) => f1 == f2 && s1 == s2
      case _ => false
    })
  }

  private def isCompatibleTypes(typ1: Type, typ2: Type): Boolean =
    isWeakerType(typ1, typ2) || isWeakerType(typ2, typ1)

  private def typeErrorMsg(
     situation: String, context: String, expected: String, got: String
  ): String =
    s"Type mismatch error in $situation\n" +
      s"  in $context\n" +
      s"  Expected '$expected', but got '$got'\n"
}
