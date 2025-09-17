package hkmc2.ctml.test

import hkmc2.Diagnostic.Source
import hkmc2.ErrorReport
import hkmc2.Raise
import hkmc2.ctml.core.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.debug.*
import hkmc2.ctml.core.type_.impls.*
import hkmc2.ctml.types.*
import hkmc2.ctml.types.given
import hkmc2.ctml.util.*
import hkmc2.semantics.Term
import sourcecode.{FileName,Line}

/** Run a CTML test on an input term. */
def test(
  term: Term,
  ctx: Context,
  import_ : Boolean,
  outputter: (String) => Unit,
  raiser: Raise,
): Context =
  // Do not output results in import files (such as the CTML prelude).
  val output = if !import_
    then (message)   => outputter(message)
    else (_: String) => ()

  val raise = (ln: Line, fn: FileName) ?=> (source: Source, message: String) =>
    raiser(ErrorReport(List((message, None)), source = source))

  val tester = Tester(ctx, output, raise)
  tester.test(term)
  tester.ctx

class Tester(
  var ctx: Context,
  output: (String) => Unit,
  raise: (Line, FileName) ?=> (Source, String) => Unit,
  prettyCtx: PrettyContext = PrettyContext()
):
  /** Run a CTML test on an input term. */
  def test(term: Term): Unit =
    // Assign global CTML debug output function.
    DebugInfo.output = output

    // Try to parse the input term.
    val stmts = try
      parseStmts(term)
    catch
      case error: ParseError =>
        raise(Source.Parsing, error.getMessage())
        return

    try
      for stmt <- stmts do
        testStatement(stmt)
    catch
      case error: TypeError =>
        raise(Source.Typing, error.prettify(prettyCtx).getMessage())
        // output(getStackTraceString(error))
      case error: Throwable =>
        output(getStackTraceString(error))
        throw error

  /** Test a statement */
  def testStatement(stmt: Stmt) =
    stmt match
      case StmtClassDecl(name) =>
        this.testClassDecl(name)
      case StmtTypeDecl(name) =>
        this.testTypeDecl(name)
      case StmtTypeVar(name, type_) =>
        this.testTypeVar(name, type_)
      case StmtExprDecl(name, type_) =>
        this.testExprDecl(name, type_)
      case StmtExprVar(name, expr) =>
        this.testExprVar(name, expr)
      case StmtExpr(expr) =>
        this.testExpr(expr)
      case StmtTypeRel(rel, left, right) =>
        this.testTypeRel(rel, left, right)

  /** Add a class to the context. */
  def testClassDecl(name: String) =
    val var_ = TypeVar(name)
    this.ctx = this.ctx.extend(TypeVarDecl(var_, TypeVarKind.Class))

  /** Add a type variable to the context. */
  def testTypeDecl(name: String) =
    val var_ = TypeVar(name)
    this.ctx = this.ctx.extend(TypeVarDecl(var_, TypeVarKind.Rigid))

  /** Add a type alias to the context. */
  def testTypeVar(name: String, type_ : Type) =
    this.output(s"${name} = ${type_.prettify(prettyCtx)}")
    val var_ = TypeVar(name)
    this.ctx = this.ctx.extend(
      TypeVarDecl(var_, TypeVarKind.Rigid),
      Bound(var_, Direction.Sub,   type_),
      Bound(var_, Direction.Super, type_),
    )

  /** Add an expression variable to the context. */
  def testExprDecl(name: String, type_ : Type) =
    this.output(s"${name}: ${type_.prettify(prettyCtx)}")
    this.ctx = this.ctx.extend(TermVarDecl(name, type_))

  /** Test an expression variable type inference and add it to the context. */
  def testExprVar(name: String, expr: Expr) =
    val (type_, bounds) = infer(expr)(using this.ctx)
    this.output(s"${name}: ${type_.prettify(prettyCtx)}")
    this.ctx = this.ctx.extend(TermVarDecl(name, type_))

  /** Test an expression type inference. */
  def testExpr(expr: Expr) =
    val (type_, outs) = infer(expr)(using this.ctx)
    this.outputType(type_)
    this.outputClauses(outs)

  /** Test the relation between two types. */
  def testTypeRel(rel: TypeRel, left: Type, right: Type) =
    val outs = rel match
      case TypeRel.Sub =>
        testSubtyping(left, right)
      case TypeRel.Sup =>
        testSupertyping(right, left)
      case TypeRel.Eq =>
        testTypeEquivalence(left, right)
      case TypeRel.Ne =>
        testTypeIncomparability(left, right)

    this.output("OK")
    this.outputClauses(outs)

  /** Test subtyping between two types. */
  def testSubtyping(sub: Type, sup: Type): Clauses =
    subtype(sub, sup)(using this.ctx)

  /** Test supertyping between two types. */
  def testSupertyping(sup: Type, sub: Type): Clauses =
    subtype(sub, sup)(using this.ctx)

  /** Test equivalence between two types. */
  def testTypeEquivalence(left: Type, right: Type): Clauses =
    given Context = this.ctx
    try
      val subClauses = subtype(left, right)
      subtypeSeq(right, left, subClauses)
    catch
      case error: TypeError =>
        error.addStep(TypeEquivalenceJudgment(left, right))
        throw error

  /** Test incomparability between two types. */
  def testTypeIncomparability(left: Type, right: Type): Clauses =
    if !checkEqual(left, right)(using this.ctx) then
      val error = TypeError()
      error.addStep(TypeIncomparabilityJudgment(left, right))
      throw error

    Clauses.empty

  /** Output the inferred type. */
  def outputType(type_ : Type) =
    this.output(type_.prettify(this.prettyCtx).show)

  /** Output the generated type bounds if there are some. */
  def outputClauses(clauses: Clauses) =
    if clauses != Clauses.empty then
      this.output(clauses.prettify(this.prettyCtx).show)
