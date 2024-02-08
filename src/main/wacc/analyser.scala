package src.main.wacc

import src.main.wacc.ErrMsg._
import scala.annotation.tailrec

object analyser {

  // This symbol table holds functions and is an ancestor to all other tables
  private val rootSymbolTable: SymbolTable = SymbolTable(None)

  def analyse(program: Program): String = {
    val error = new StringBuilder()
    rootSymbolTable.clear() // Empty the table for subsequent calls to analyse

    // Functions may be used before declaration, so we need to do a first pass
    for (f <- program.functions) {
      if (rootSymbolTable.inCurrentScope(f.ident))
        error ++= s"Attempted redeclaration of function '${f.ident}'\n"
      else
        rootSymbolTable.put(f.ident, f)
    }

    // Check validity of function parameters and body
    for (f <- program.functions) {
      // Each function will have its own scope
      val symTable = rootSymbolTable.makeChild
      f.params.foreach(p =>
        if (symTable.inCurrentScope(p.ident))
          error ++=
            s"Attempted redeclaration of parameter '${p.ident}'\n" withContext s"function $f"
        else {
          symTable.put(p.ident, p)
        }
      )
      error ++= checkFuncStmt(symTable.makeChild, f.body, f.t) withContext s"function $f"
    }

    // Check the main program body
    error ++= checkMainStmt(rootSymbolTable.makeChild, program.body)

    error.toString
  }

  // Distinguish between function and main statements since return is not allowed in main
  private def checkFuncStmt(st: SymbolTable, stmt: Stmt, typ: Type): String = stmt match {
    case Return(expr) =>
      val (err, expType) = checkExpr(st, expr)
      // Check that the return expression is valid and matches the function return type
      (err withContext stmt) ++
        (if (expType.isDefined && !isWeakerType(typ, expType.get)) {
           typeErrorMsg("function return", s"return $expr", s"$typ", s"${expType.get}") withContext
             stmt
         } else "")
    // If and while statements have a condition that must be a boolean
    case IfStmt(cond, body1, body2) =>
      checkCond(st, cond, isIf = true) ++
        checkFuncStmt(st.makeChild, body1, typ) ++ checkFuncStmt(st.makeChild, body2, typ)
    case While(cond, body) =>
      checkCond(st, cond, isIf = false) ++ checkFuncStmt(st.makeChild, body, typ)
    case ScopedStmt(stmt)      => checkFuncStmt(st.makeChild, stmt, typ)
    case StmtChain(stmt, next) => checkFuncStmt(st, stmt, typ) ++ checkFuncStmt(st, next, typ)
    case _                     => checkLeafStatement(st, stmt)
  }

  // Main program body
  private def checkMainStmt(st: SymbolTable, stmt: Stmt): String = stmt match {
    case Return(_) => s"Return not allowed in main\n" withContext stmt
    case IfStmt(cond, body1, body2) =>
      checkCond(st, cond, isIf = true) ++
        checkMainStmt(st.makeChild, body1) ++ checkMainStmt(st.makeChild, body2)
    case While(cond, body) => checkCond(st, cond, isIf = false) ++ checkMainStmt(st.makeChild, body)
    case ScopedStmt(stmt)  => checkMainStmt(st.makeChild, stmt)
    case StmtChain(stmt, next) => checkMainStmt(st, stmt) ++ checkMainStmt(st, next)
    case _                     => checkLeafStatement(st, stmt)
  }

  // Used in if and while statements to check that the condition is a boolean
  private def checkCond(st: SymbolTable, cond: Expr, isIf: Boolean) = {
    val (err, typ) = checkExpr(st, cond)
    err ++ (if (typ.isDefined && typ.get != BoolType)
              typeErrorMsg(
                (if (isIf) "if statement" else "loop") ++ " conditional",
                if (isIf) s"if $cond then" else s"while $cond do",
                "bool",
                s"${typ.get}"
              )
            else "")
  }

  // Checks the validity of statements that cannot have any other statements as children
  private def checkLeafStatement(st: SymbolTable, stmt: Stmt): String = stmt match {
    case Skip                 => ""
    case Decl(t, name, value) => handleDeclaration(st, t, name, value)
    case Asgn(left, value)    => checkAssignment(st, left, value)
    case Read(value)          => checkRead(st, value)
    case Free(expr)           => checkFree(st, expr)
    case Exit(expr) =>
      checkExpr(st, expr) match { // expects an int
        case (err, Some(t)) if t != IntType =>
          (err withContext stmt) ++
            typeErrorMsg("exit statement", s"exit $expr", "int", s"$t")
        case (err, _) => err
      }
    case Print(expr)   => checkExpr(st, expr)._1
    case PrintLn(expr) => checkExpr(st, expr)._1
    // Should never happen - if it does, it's a bug
    case _ => throw new IllegalArgumentException("Non-leaf statement in checkLeafStatement\n")
  }

  // Checks a declaration is valid and adds it to the symbol table
  private def handleDeclaration(
      symTable: SymbolTable,
      typ: Type,
      ident: Ident,
      value: RVal
  ): String = {
    val error = new StringBuilder()

    // Check and find the type of the right hand side
    val (err, typ2) = checkRVal(symTable, value)
    error ++= err withContext s"$typ $ident = $value"
    // Check that the type of the right hand side is compatible with the declared type
    if (typ2.isDefined && !isWeakerType(typ, typ2.get)) {
      error ++= typeErrorMsg(
        s"declaration of variable $ident",
        s"$typ $ident = $value",
        s"$typ",
        s"${typ2.get}"
      )
    }

    // Do not allow redeclaration
    if (symTable.inCurrentScope(ident))
      error ++= s"Attempted redeclaration of variable '$ident' in same scope\n" withContext
        s"$typ $ident = $value"
    else {
      // If the right hand side is a variable, we use its symbol table entry rather than the Ident
      value match {
        case id: Ident =>
          checkIdent(symTable, id) match {
            case Left(err) => error ++= err withContext s"$typ $ident = $value"
            case Right(typ2) =>
              if (isWeakerType(typ, typ2)) symTable.put(ident, symTable(id).get)
          }
        case _ => symTable.put(ident, value)
      }
    }

    value.typ = Some(typ) // Set the type of the symbol table entry
    error.toString
  }

  // Checks that an assignment is valid
  private def checkAssignment(symTable: SymbolTable, left: LVal, value: RVal): String = {
    val error = new StringBuilder()
    var typ1: Option[Type] = None
    checkLVal(symTable, left) match { // Check that the left hand side is a valid lvalue
      case Left(err)  => error ++= err withContext s"$left = $value"
      case Right(typ) => typ1 = Some(typ)
    }
    val (err, typ2) = checkRVal(symTable, value)
    error ++= err withContext s"$left = $value"
    // Check that the types are compatible
    if (typ1.isDefined && typ2.isDefined && !isWeakerType(typ1.get, typ2.get))
      error ++= typeErrorMsg("assignment", s"$left = $value", s"${typ1.get}", s"${typ2.get}")
    error.toString
  }

  // Checks that a read statement is valid
  private def checkRead(st: SymbolTable, value: LVal): String = checkLVal(st, value) match {
    case Left(err) => err
    case Right(typ) =>
      typ match {
        case IntType | CharType => ""
        case _ => // Only int and char are allowed
          typeErrorMsg("read statement", s"read $value", "int' or 'char", s"$typ")
      }
  }

  // Checks that a free statement is valid
  private def checkFree(st: SymbolTable, expr: Expr) = {
    checkExpr(st, expr) match {
      case (err, Some(PairType(_, _))) => err
      case (err, Some(ArrayType(_)))   => err
      case (err, Some(Pair))           => err
      case (err, None)                 => err
      // We can only free pairs and arrays
      case (err, Some(typ)) =>
        err ++ typeErrorMsg("free statement", s"free $expr", "pair' or 'array", s"$typ")
    }
  }

  // Checks the validity of an expression and finds it type if possible
  @tailrec
  private def checkExpr(symTable: SymbolTable, expr: Expr): (String, Option[Type]) = expr match {
    case Integer(_)    => ("", Some(IntType))
    case Bool(_)       => ("", Some(BoolType))
    case Character(_)  => ("", Some(CharType))
    case StringAtom(_) => ("", Some(StringType))
    case Null          => ("", Some(Pair))
    case id: Ident =>
      checkIdent(symTable, id) match {
        case Left(err)  => (err withContext expr, None)
        case Right(typ) => ("", Some(typ))
      }
    // Array indexing
    case ArrayElem(ident, exprs) =>
      checkArrayElem(symTable, ident, exprs) match {
        case Left(err)  => (err, None)
        case Right(typ) => ("", Some(typ))
      }
    case BracketedExpr(expr) => checkExpr(symTable, expr)
    // Unary and binary operators mutually recursive with this function
    case UnaryApp(op, expr)         => checkUnaryApp(symTable, op, expr)
    case BinaryApp(op, left, right) => checkBinaryApp(symTable, op, left, right)
  }

  // Checks that an identifier is defined and returns its type if possible
  private def checkIdent(symTable: SymbolTable, ident: Ident): Either[String, Type] =
    symTable(ident) match {
      case None                   => Left(s"Variable '$ident' used before declaration!\n")
      case Some(Func(_, _, _, _)) => Left(s"Function '$ident' used as variable!\n")
      case Some(obj) =>
        assert(obj.typ.isDefined) // Everything in symbol table should have a type
        Right(obj.typ.get)
    }

  // Attempts to evaluate constant integer expressions
  private def evalConst(expr: Expr): Option[BigInt] = expr match {
    case Integer(i)                      => Some(i)
    case UnaryApp(Neg, e)                => evalConst(e).map(-_)
    case UnaryApp(Ord, Character(c))     => Some(c.toInt)
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
          // Do not attempt division by 0
          case Div => if (v2 == 0) None else Some(v1 / v2)
          case Mod => Some(v1 % v2)
          case _   => None
        }
      } else None
    case _ => None
  }

  // Uses evalConst to check for overflow in constant integer expressions
  private def checkConstantApplication(left: Expr, right: Expr, op: BinaryOp): String = {
    val leftv = evalConst(left)
    val rightv = evalConst(right)
    if (leftv.isDefined && rightv.isDefined) {
      val lv = leftv.get
      val rv = rightv.get
      op match {
        // Negative value could also cause underflow
        case Add =>
          if (lv + rv > Int.MaxValue || lv + rv < Int.MinValue)
            s"Overflow error in expression: $left + $right:\n" ++
              "  Addition of int literals would result in overflow\n"
          else ""
        case Sub =>
          if (lv - rv < Int.MinValue || lv - rv > Int.MaxValue) {
            s"Overflow error in expression: $left - $right:\n" ++
              "  Subtraction of int literals would result in underflow\n"
          } else ""
        case Mul =>
          if (lv * rv > Int.MaxValue || lv * rv < Int.MinValue)
            s"Overflow error in expression: $left * $right:\n" ++
              "  Multiplication of int literals would result in overflow\n"
          else ""
        // Division by 0 is a runtime error, so we don't check it here
        case _ => ""
      }
    } else ""
  }

  // Error messages for unary operator applications
  private def unaryAppErrMsg(op: UnaryOp, typ: Type, expr: Expr): String = {
    val expected: String = op match {
      case Chr | Neg => IntType.toString
      case Ord       => CharType.toString
      case Len       => s"$StringType or array"
      case Not       => BoolType.toString
    }
    typeErrorMsg(s"application of $op operator", s"expression: $expr", expected, s"$typ")
  }

  // Checks the validity of unary operator applications and returns the type
  private def checkUnaryApp(
      symTable: SymbolTable,
      op: UnaryOp,
      expr: Expr
  ): (String, Option[Type]) = {
    val error = new StringBuilder()
    val (err, typ) = checkExpr(symTable, expr)
    error ++= err withContext s"$op $expr"

    var retType: Option[Type] = None // The type of the expression after the operator is applied
    if (typ.isDefined) {
      val someType = typ.get
      op match { // Check that the type is compatible with the operator
        case Chr =>
          if (someType == IntType) retType = Some(CharType)
          else error ++= unaryAppErrMsg(Chr, someType, expr)
        case Len =>
          if (someType == StringType || someType.isInstanceOf[ArrayType]) retType = Some(IntType)
          else error ++= unaryAppErrMsg(Len, someType, expr)
        case Neg =>
          if (someType == IntType) {
            // Check for overflow
            if (evalConst(expr).contains(Int.MinValue))
              error ++= "Negation of int literal would result in overflow\n" ++
                s"  in expression: $expr\n"
            retType = Some(IntType)
          } else error ++= unaryAppErrMsg(Neg, someType, expr)
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

  // Error messages for binary operator applications
  private def binaryAppErrMsg(op: BinaryOp, typ1: Type, typ2: Type, expr: Expr): String = {
    val expected: String = op match {
      case And | Or                                      => BoolType.toString
      case Eq | NotEq                                    => "compatible types"
      case Add                                           => s"$IntType' or '$StringType"
      case Gt | GtEq | Lt | LtEq | Sub | Mul | Div | Mod => IntType.toString
    }
    typeErrorMsg(
      s"application of '$op' operator",
      s"expression: $expr",
      expected,
      s"$typ1' and '$typ2"
    )
  }

  // Checks the validity of binary operator applications and returns the type
  private def checkBinaryApp(
      symTable: SymbolTable,
      op: BinaryOp,
      left: Expr,
      right: Expr
  ): (String, Option[Type]) = {
    val error = new StringBuilder()
    // First validate sub-expressions
    val (err1, typ1) = checkExpr(symTable, left)
    val (err2, typ2) = checkExpr(symTable, right)
    error ++= err1 withContext s"$left $op $right"
    error ++= err2 withContext s"$left $op $right"
    var retType: Option[Type] = None // The type of the expression after the operator is applied
    if (typ1.isDefined && typ2.isDefined) {
      val someType1 = typ1.get
      val someType2 = typ2.get
      op match { // Check that the types are compatible with the operator
        case And | Or =>
          if (someType1 == BoolType && someType2 == BoolType) retType = Some(BoolType)
          else error ++= binaryAppErrMsg(op, someType1, someType2, BinaryApp(op, left, right))
        case Eq | NotEq =>
          if (isCompatibleTypes(someType1, someType2)) retType = Some(BoolType)
          else error ++= binaryAppErrMsg(op, someType1, someType2, BinaryApp(op, left, right))
        case Gt | GtEq | Lt | LtEq =>
          if (
            someType1 == IntType && someType2 == IntType ||
            someType1 == CharType && someType2 == CharType
          ) retType = Some(BoolType)
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

  // Checks array indexing and returns the type of the expression if valid
  private def checkArrayElem(
      symTable: SymbolTable,
      ident: Ident,
      exprs: List[Expr]
  ): Either[String, Type] = {
    val error = new StringBuilder()
    var typ: Option[Type] = None
    checkIdent(symTable, ident) match { // Check that we are accessing an array
      case Left(err) => error ++= err withContext s"$ident[${exprs.mkString("][")}]"
      case Right(ArrayType(typ2)) =>
        legalArrayDimAccess(ArrayType(typ2), exprs) match {
          case None =>
            error ++= s"Invalid array access: mismatching dimensions\n" ++
              s"  in $ident[${exprs.mkString("][")}]\n"
          case Some(t) => typ = Some(t)
        }
      case Right(typ2) =>
        error ++= typeErrorMsg(
          "array access",
          s"$ident[${exprs.mkString("][")}]",
          "array",
          s"$typ2"
        )
    }
    // Check that the array indices are valid
    // We don't check for out of bounds, as this is a runtime error
    exprs.foreach(expr => {
      val (err, typ2) = checkExpr(symTable, expr)
      error ++= err withContext s"$ident[${exprs.mkString("][")}]"
      if (err.nonEmpty) error ++= s"  in $ident[${exprs.mkString("][")}]\n"
      if (typ2.isDefined && typ2.get != IntType)
        error ++= typeErrorMsg(
          "array index",
          s"$ident[${exprs.mkString("][")}]",
          "int",
          s"${typ2.get}"
        )
    })
    if (error.isEmpty) Right(typ.get) else Left(error.toString)
  }

  // Checks that sub-arrays exist if the user is attempting to access them
  @tailrec
  private def legalArrayDimAccess(typ: Type, exprs: List[Expr]): Option[Type] =
    (typ, exprs) match {
      case (ArrayType(typ2), _ :: tail) =>
        legalArrayDimAccess(typ2, tail)
      case (t, Nil) => Some(t)
      case (_, _)   => None
    }

  // Checks that the left hand side of an assignment or declaration is valid
  // and returns its type if valid
  private def checkLVal(symTable: SymbolTable, lval: LVal): Either[String, Type] = lval match {
    case id: Ident               => checkIdent(symTable, id)
    case ArrayElem(ident, exprs) => checkArrayElem(symTable, ident, exprs)
    case Fst(value) =>
      checkLVal(symTable, value) match {
        // The type of fst is the type of the first element of the pair
        case Left(err)               => Left(err)
        case Right(PairType(typ, _)) => Right(typ)
        case Right(Pair) => Right(NullType) // We don't know the type of the pair, but it is valid
        case Right(typ) => Left(typeErrorMsg("pair element access", s"fst $value", "pair", s"$typ"))
      }
    case Snd(value) =>
      checkLVal(symTable, value) match {
        case Left(err)               => Left(err)
        case Right(PairType(_, typ)) => Right(typ)
        case Right(Pair) => Right(NullType) // We don't know the type of the pair, but it is valid
        case Right(typ) => Left(typeErrorMsg("pair element access", s"snd $value", "pair", s"$typ"))
      }
  }

  // Checks the validity of the right hand side of an assignment or declaration
  // and returns its type if valid
  private def checkRVal(symTable: SymbolTable, value: RVal): (String, Option[Type]) = value match {
    case ArrayLiter(exprs)     => checkArrayLiteral(symTable, exprs)
    case NewPair(expr1, expr2) => checkNewPair(symTable, expr1, expr2)
    case exp: Expr             => checkExpr(symTable, exp)
    // Ident, Fst and Snd match this, Ident is already handled in checkExpr
    case fstSnd: LVal =>
      checkLVal(symTable, fstSnd) match {
        case Left(err)  => (err, None) // couldn't infer type
        case Right(typ) => ("", Some(typ))
      }
    // Function call
    case Call(ident, exprs) => checkCall(symTable.makeChild, ident, exprs)
  }

  // Checks the validity of newpair expressions, found in the right hand side of declarations
  private def checkNewPair(symTable: SymbolTable, expr1: Expr, expr2: Expr) = {
    // First check sub-expressions
    val (err1, typ1) = checkExpr(symTable, expr1)
    val (err2, typ2) = checkExpr(symTable, expr2)
    val error = new StringBuilder()
    error ++= err1 withContext s"newpair($expr1, $expr2)"
    error ++= err2 withContext s"newpair($expr1, $expr2)"
    if (typ1.isDefined && typ2.isDefined) {
      // Nested pair types are erased to Pair
      val ltype =
        if (!typ1.get.isInstanceOf[PairElemType]) Pair
        else typ1.get.asInstanceOf[PairElemType]
      val rtype =
        if (!typ2.get.isInstanceOf[PairElemType]) Pair
        else typ2.get.asInstanceOf[PairElemType]
      (error.toString(), Some(PairType(ltype, rtype)))
    } else (error.toString, None)
  }

  // Checks the validity of array literals and returns their type if valid
  private def checkArrayLiteral(
      symTable: SymbolTable,
      exprs: List[Expr]
  ): (String, Option[Type]) = {
    val errors = new StringBuilder()
    // Check each sub-expression
    val errTyps = exprs.map(expr => checkExpr(symTable, expr))
    for ((err, _) <- errTyps) {
      errors ++= err withContext s"[${exprs.mkString(", ")}]"
    }
    // Check the types of the sub-expressions are compatible
    val typs = errTyps.map(_._2)
    if (typs.nonEmpty && typs.forall(_.isDefined)) {
      var typ = typs.head.get
      for (t <- typs.tail) {
        if (!isWeakerType(typ, t.get)) {
          if (isWeakerType(t.get, typ)) {
            typ = t.get // Weaken current type
          } else {
            errors ++= typeErrorMsg(
              "array literal",
              s"[${exprs.mkString(", ")}]",
              s"$typ",
              s"${t.get}"
            )
            (errors.toString, None)
          }
        }
      }
      (errors.toString, Some(ArrayType(typ)))
    } else (errors.toString, None)
  }

  // Checks the validity of function calls and returns their type if valid
  private def checkCall(symTable: SymbolTable, ident: Ident, exprs: List[Expr]) = {
    // Functions will only ever be declared in the root symbol table
    rootSymbolTable(ident) match {
      case None => (s"Usage of undeclared function: $ident!\n", None)
      case Some(Func(typ, _, params, _)) =>
        val errors = new StringBuilder()
        // Check the parameters match
        if (params.length != exprs.length)
          errors ++=
            s"Incorrect number of arguments in function call:\n" +
              s"  in call $ident(${exprs.mkString(", ")})\n"
        // Check the type of the parameters
        for ((param, expr) <- params.zip(exprs)) {
          val (err, ptype) = checkExpr(symTable, expr)
          errors ++= err withContext s"call $ident(${exprs.mkString(", ")})"
          if (ptype.isDefined) {
            if (!isWeakerType(param.t, ptype.get))
              errors ++= typeErrorMsg(
                s"function argument ${param.ident.name}",
                s"$ident(${exprs.mkString(", ")})",
                s"${param.t}",
                s"${ptype.get}"
              )
            else symTable.put(param.ident, expr)
          }
        }
        (errors.toString, Some(typ))
      // This should never happen - there should only be functions in the root symbol table
      case Some(_) => throw new IllegalArgumentException("Non-function in root symbol table\n")
    }
  }

  // Checks that the left type is compatible with the right
  private def isWeakerType(weaker: Type, stronger: Type): Boolean = {
    ((weaker != NullType || stronger != NullType) &&
      (weaker == stronger || weaker == NullType || stronger == NullType)) ||
    (weaker == Pair && stronger.isInstanceOf[PairType]) ||
    (stronger == Pair && weaker.isInstanceOf[PairType]) ||
    weaker == StringType && stronger == ArrayType(CharType) ||
    ((weaker, stronger) match {
      // Array and pair types have invariant type parameters
      case (ArrayType(t1), ArrayType(t2)) =>
        t1 == t2 ||
        t1 == Pair && t2.isInstanceOf[PairType] ||
        t2 == Pair && t1.isInstanceOf[PairType]
      case (PairType(f1, s1), PairType(f2, s2)) => f1 == f2 && s1 == s2
      case _                                    => false
    })
  }

  // Checks types are compatible
  private def isCompatibleTypes(typ1: Type, typ2: Type): Boolean =
    isWeakerType(typ1, typ2) || isWeakerType(typ2, typ1)
}
