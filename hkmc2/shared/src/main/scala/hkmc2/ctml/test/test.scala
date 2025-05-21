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
    // Run the type inference or type constraining test.
    stmt match
      case StmtExprDecl(name, type_) =>
        this.addExprDecl(name, type_)
      case StmtTypeDecl(name) =>
        this.addTypeDecl(name)
      case StmtExpr(expr) =>
        this.testExpr(expr)
      case StmtTypeRel(rel, left, right) =>
        this.testTypeRel(rel, left, right)

  /** Add a variable declaration to the context. */
  def addExprDecl(name: String, type_ : Type) =
    this.ctx = CtxVar(name, type_) :: this.ctx

  /** Add a type declaration to the context. */
  def addTypeDecl(name: String) =
    this.ctx = CtxTypeVar(name, TypeVarKind.Rigid) :: this.ctx

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
