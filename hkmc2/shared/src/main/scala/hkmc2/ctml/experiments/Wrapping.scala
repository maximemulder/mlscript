package hkmc2.ctml.experiments

import hkmc2.ctml.config.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.inference.*
import hkmc2.ctml.core.type_.impls.*
import hkmc2.ctml.core.var_.resetFreshVarCounter
import hkmc2.ctml.types.*

/** An isolated comparison of ordinary inference before and after administrative wrapping.
 *  This entry point bypasses the MLscript parser, whose scrutinee bindings would otherwise
 *  confound the comparison. It does not change the type checker's default inference policy. */
object Wrapping:
  /** The three input transformations compared by the experiment. */
  enum Variant:
    /** Join locally and carry the resulting bounds to the enclosing inference level. */
    case bubble
    /** Insert an unused-parameter lambda applied to a unit value around each match. */
    case dummy
    /** Rebind a variable scrutinee around each match, refining its occurrences together. */
    case rebind

  /** Rewrite every source match once, including matches without an else branch. */
  def elaborate(expr: Expr, variant: Variant): Expr =
    expr match
      case expr: EVar => expr
      case ETuple(left, right) =>
        ETuple(elaborate(left, variant), elaborate(right, variant))
      case ELam(name, body) => ELam(name, elaborate(body, variant))
      case EApp(lam, arg) => EApp(elaborate(lam, variant), elaborate(arg, variant))
      case EAscr(body, type_) => EAscr(elaborate(body, variant), type_)
      case EMatch(scrutinee, pattern, then_, else_) =>
        val body = EMatch(elaborate(scrutinee, variant), pattern,
          elaborate(then_, variant), else_.map(elaborate(_, variant)))
        variant match
          case Variant.bubble => body
          case Variant.dummy =>
            // Avoid every name in the body, including bound names, so the binder is unused.
            val names = termNames(body)
            val name = Iterator.from(0).map(i => s"wrappingDummy$i")
              .find(name => !names.contains(name)).get
            EApp(ELam(name, body), EVar("unit"))
          case Variant.rebind =>
            scrutinee match
              case EVar(name) => EApp(ELam(name, body), EVar(name))
              case _ =>
                throw IllegalArgumentException("Rebinding requires a variable scrutinee")

  /** All term names in an expression, used to choose an unused dummy binder. */
  private def termNames(expr: Expr): Set[String] =
    expr match
      case EVar(name) => Set(name)
      case ETuple(left, right) => termNames(left) ++ termNames(right)
      case ELam(name, body) => termNames(body) + name
      case EApp(lam, arg) => termNames(lam) ++ termNames(arg)
      case EAscr(body, _) => termNames(body)
      case EMatch(scrutinee, _, then_, else_) =>
        termNames(scrutinee) ++ termNames(then_) ++ else_.toSet.flatMap(termNames)

  /** A named fixture and its intended acceptance, independent of the inference strategy. */
  case class Fixture(name: String, expr: Expr, expected: String = "ok")

  /** Class types used in place of the Lean probe's C, S, and D. */
  private def cls(name: String): Type = TClass(ClassVar(name))

  /** A binary class test. */
  private def test(name: String, then_ : Expr, else_ : Expr): Expr =
    EMatch(EVar(name), cls("Int"), then_, Some(else_))

  /** The practical suite's exhaustive Int/Str split, with its final one-branch match. */
  private def flowBody(a: String, b: String): Expr =
    test(a, EAscr(EVar(b), cls("Int")),
      EMatch(EVar(a), cls("Str"), EAscr(EVar(b), cls("Str")), None))

  /** Calls are ascribed so that an accepted caller must also have the expected result type. */
  private def call(expr: Expr, args: List[Expr], result: Type): Expr =
    EAscr(args.foldLeft(expr)(EApp.apply), result)

  /** Closed fixtures in the primitive term environment; callees are inlined unless named let. */
  def fixtures: List[Fixture] =
    val int = EVar("int")
    val str = EVar("string")
    val bool = EVar("bool")
    val i = cls("Int")
    val s = cls("Str")
    val union = TJointType(JointMode.Union, i, s)
    val flow = ELam("a", test("a", int, str))
    // A partial operation on b replaces record selection, absent from the practical language.
    val bad = ELam("a", ELam("b", test("a", EAscr(EVar("b"), i), int)))
    val sel = ELam("a", ELam("b",
      test("a", test("b", int, EAscr(EVar("b"), TBot)), int)))
    val foo = ELam("a", flowBody("a", "a"))
    val foo2 = ELam("a", ELam("b", flowBody("a", "b")))
    val overload = TJointType(JointMode.Inter, TLam(i, i), TLam(s, s))
    val letCalls = EApp(ELam("f", ETuple(
      call(EVar("f"), List(int), i), call(EVar("f"), List(str), s))), foo)
    val capture = ELam("a", ELam("wrappingDummy0", test("a", EVar("wrappingDummy0"), str)))
    List(
      Fixture("flow-infer", flow),
      Fixture("flow-then", call(flow, List(int), i)),
      Fixture("flow-else", call(flow, List(str), s)),
      Fixture("flow-other", call(flow, List(bool), s)),
      Fixture("flow-wrong", EAscr(flow, TLam(i, s)), "failed"),
      Fixture("flow-overload", EAscr(flow, overload)),
      Fixture("flow-union", call(flow, List(EVar("intOrStr")), union)),
      Fixture("bad-infer", bad),
      Fixture("bad-honest-then", call(bad, List(int, int), i)),
      Fixture("bad-honest-else", call(bad, List(bool, str), i)),
      Fixture("bad-dishonest", call(bad, List(int, str), i), "failed"),
      Fixture("sel-infer", sel),
      Fixture("sel-then", call(sel, List(int, int), i)),
      Fixture("sel-else", call(sel, List(str, bool), i)),
      Fixture("sel-dishonest", call(sel, List(int, bool), i), "failed"),
      Fixture("foo-infer", foo),
      Fixture("foo-then", call(foo, List(int), i)),
      Fixture("foo-else", call(foo, List(str), s)),
      Fixture("foo-dishonest", EApp(foo, bool), "failed"),
      Fixture("foo-overload", EAscr(foo, overload)),
      Fixture("foo2-infer", foo2),
      Fixture("foo2-then", call(foo2, List(int, int), i)),
      Fixture("foo2-else", call(foo2, List(str, str), s)),
      Fixture("foo2-dishonest", EApp(EApp(foo2, int), str), "failed"),
      Fixture("foo2-independent-unions", call(foo2,
        List(EAscr(int, union), EAscr(str, union)), union), "failed"),
      Fixture("let-polymorphic", letCalls),
      Fixture("concrete-union", test("intOrStr", int, str)),
      Fixture("capture", capture),
      Fixture("capture-then", call(capture, List(int, bool), cls("Bool"))),
    ) ::: List(1, 2, 4, 8).map(n =>
      Fixture(s"siblings-$n", ELam("a", siblings(n)))
    ) ::: List(1, 2, 4, 8).map(n =>
      Fixture(s"nested-$n", ELam("a", nested(n)))
    )

  /** Sibling tests whose joined bounds meet in one enclosing abstraction. */
  private def siblings(n: Int): Expr =
    if n == 0 then EVar("int")
    else ETuple(test("a", EVar("int"), EVar("string")), siblings(n - 1))

  /** A chain of nested tests, each retaining the outer branch's assumptions. */
  private def nested(n: Int): Expr =
    if n == 0 then EVar("int")
    else test("a", nested(n - 1), EVar("string"))

  /** The primitive context; all declared constants denote values, including the dummy unit. */
  private def context: TypeContext =
    val sub = List("Int", "Str", "Bool", "Unit").foldLeft(SubContext.empty)(
      (ctx, name) => ctx.extend(ClassDecl(name, None)))
    TypeContext(sub, List(
      TermVarDecl("int", cls("Int")), TermVarDecl("string", cls("Str")),
      TermVarDecl("bool", cls("Bool")), TermVarDecl("unit", cls("Unit")),
      TermVarDecl("intOrStr", TJointType(JointMode.Union, cls("Int"), cls("Str"))),
    ))

  /** Count tree occurrences, including types inside guards, without expanding type variables. */
  private def size(type_ : Type): Int = 1 + type_.components.map(size).sum

  /** A measured inference outcome; a resource limit is distinct from a typing rejection. */
  private case class Result(outcome: String, nodes: Int, describe: () => String)

  /** Infer with a fresh configuration and counter for every sample. */
  private def run(expr: Expr, ctx: TypeContext): Result =
    resetConfig()
    resetFreshVarCounter()
    config.checkUnsolvableConstreds = true
    try
      val (type_, outs) = inferTopLevel(expr)(using ctx)
      assert(outs.elems.isEmpty, "Top-level inference must package its output clauses")
      Result("ok", size(type_), () => type_.prettify(PrettyContext()).toString)
    catch
      case error: TypeError =>
        Result("failed", 0, () => error.prettify(PrettyContext()).getMessage())
      case error: Exception if error.getMessage == "Exceeded maximum call depth." =>
        Result("depthLimit", 0, () => error.getMessage)
      case error: Exception if error.getMessage == "Exceeded maximum step count." =>
        Result("stepLimit", 0, () => error.getMessage)

  /** List fixtures or run one cell; the external driver enforces a process timeout. */
  def main(args: Array[String]): Unit =
    args.toList match
      case List("--list") => fixtures.foreach(f => println(s"${f.name},${f.expected}"))
      case List(mode, name, warmups, repetitions) =>
        val variant = Variant.valueOf(mode)
        val fixture = fixtures.find(_.name == name).getOrElse(
          throw IllegalArgumentException(s"Unknown fixture: $name"))
        val expr = elaborate(fixture.expr, variant)
        val ctx = context
        val warmupCount = warmups.toInt
        val repeatCount = repetitions.toInt
        require(warmupCount >= 0 && repeatCount > 0)
        for _ <- 0 until warmupCount do run(expr, ctx)
        for repetition <- 0 until repeatCount do
          val start = System.nanoTime()
          val result = run(expr, ctx)
          val millis = (System.nanoTime() - start).toDouble / 1000000
          // Type/error text is emitted separately and excluded from the CSV column structure.
          println(s"$mode,$name,$repetition,${fixture.expected},${result.outcome}," +
            s"$millis,${result.nodes}")
          if repetition == 0 then
            System.err.println(result.describe())
      case _ =>
        throw IllegalArgumentException(
          "Usage: Wrapping --list | <variant> <case> <warmups> <repeats>")
