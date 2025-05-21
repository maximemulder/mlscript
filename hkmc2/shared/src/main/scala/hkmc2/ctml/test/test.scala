package hkmc2.ctml.test

import hkmc2.ctml.core.*
import hkmc2.ctml.types.*
import hkmc2.semantics.Term

/** Run a CTML test on an input term. */
def test(term: Term, ctx: Context, output: (String) => Unit): Context =
  val tester = Tester(ctx, output)
  tester.test(term)
  tester.ctx

class Tester(var ctx: Context, output: (String) => Unit):
  /** Run a CTML test on an input term. */
  def test(term: Term): Unit =
    // Assign global CTML debug output function.
    outputter = output

    // Try to parse the input term.
    val stmts = try
      parseStmts(term)
    catch
      case error: ParseError =>
        this.output(s"PARSE ERROR: ${error.getMessage()}")
        return

    try
      // Assign the global CTML fresh variable counter.
      freshVarCounter = 0

      for stmt <- stmts do
        testStatement(stmt)
    catch
      case error: TypeError =>
        output(s"TYPE ERROR: ${error.getMessage()}")

  /** Test a statement */
  def testStatement(stmt: Stmt) =
    stmt match
      case StmtTypeVar(name) =>
        this.testTypeVar(name)
      case StmtExprVar(name, type_, expr) =>
        this.testExprVar(name, type_, expr)
      case StmtExpr(expr) =>
        this.testExpr(expr)
      case StmtTypeRel(rel, left, right) =>
        this.testTypeRel(rel, left, right)

  /** Add a type variable to the context. */
  def testTypeVar(name: String) =
    this.ctx = CtxTypeVar(name, TypeVarKind.Rigid) :: this.ctx

  /** Test an expression variable and add it to the context. */
  def testExprVar(name: String, type_ : Option[Type], expr: Option[Expr]) =
    val varType = expr match
      case Some(expr) =>
        val (inferredType, bounds) = infer(expr, this.ctx)
        type_ match
          case Some(type_) =>
            given Context = this.ctx
            checkSub(inferredType, type_)
            type_
          case None =>
            inferredType
      case None =>
        type_ match
          case Some(type_) =>
            type_
          case None =>
            TTop

    this.output(s"OK ${name}: ${varType}")
    this.ctx = CtxVar(name, varType) :: this.ctx

  /** Test an expression type inference. */
  def testExpr(expr: Expr) =
    val (type_, bounds) = infer(expr, this.ctx)
    this.outputType(type_)
    this.outputBounds(bounds)

  /** Test the relation between two types. */
  def testTypeRel(rel: TypeRel, left: Type, right: Type) =
    given Context = Context.primitive
    val bounds = rel match
      case TypeRel.Eq =>
        if !checkEq(left, right) then
          throw new TypeError("FAIL")
        Nil
      case TypeRel.Ne =>
        if checkEq(left, right) then
          throw new TypeError("FAIL")
        Nil
      case TypeRel.Sub =>
        constrainSub(left, right)
      case TypeRel.Sup =>
        constrainSub(right, left)

    this.output("OK")
    this.outputBounds(bounds)

  /** Output the inferred type. */
  def outputType(type_ : Type) =
    this.output(type_.show())

  /** Output the generated type bounds if there are some. */
  def outputBounds(bounds: List[Bound]) =
    if bounds != Nil then
      this.output(showBounds(bounds))
