package hkmc2.ctml

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

import hkmc2.ctml.config.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

/** Check solution-family preservation and integration of flexible Boolean clauses. */
class FlexibleClauseTest extends AnyFunSuite with BeforeAndAfterEach:
  /** Bound exploratory solver work independently of its proof outcome. */
  override def beforeEach(): Unit =
    super.beforeEach()
    resetConfig()
    resetFreshVarCounter()
    config.searchSteps = Some(20000)

  /** Restore configuration for neighboring suites. */
  override def afterEach(): Unit =
    try resetConfig()
    finally super.afterEach()

  /** A flexible pair whose lower bounds can be disjoint nonempty classes. */
  private def context: SubContext = SubContext.empty
    .declClass("Int", None).declClass("Str", None)
    .declTypeVar(TypeVar("A"), TypeVarKind.Flex)
    .declTypeVar(TypeVar("B"), TypeVarKind.Flex)

  /** Run every permutation, reporting rejection separately from resource exhaustion. */
  private def outcomes(constraints: List[Constraint]): List[String] =
    constraints.permutations.toList.map: ordered =>
      try
        val outs = ordered.foldLeft(SubClauses.empty)((outs, c) =>
          subtypeConstraintSeq(c, outs)(using context))
        s"accepted: $outs"
      catch
        case _: TypeError => "rejected"
        case _: SearchInconclusive => "inconclusive"

  test("disjoint nonempty requirements are accepted in every order"):
    val a = TVar(TypeVar("A"))
    val b = TVar(TypeVar("B"))
    val constraints = List(
      Constraint(TJointType(JointMode.Inter, a, b), Direction.Sub, TBot),
      Constraint(TClass(ClassVar("Int")), Direction.Sub, a),
      Constraint(TClass(ClassVar("Str")), Direction.Sub, b))
    val result = outcomes(constraints)
    assert(result.forall(_.startsWith("accepted:")))

  test("overlapping nonempty requirements are rejected in every order"):
    val a = TVar(TypeVar("A"))
    val b = TVar(TypeVar("B"))
    val constraints = List(
      Constraint(TJointType(JointMode.Inter, a, b), Direction.Sub, TBot),
      Constraint(TClass(ClassVar("Int")), Direction.Sub, a),
      Constraint(TClass(ClassVar("Int")), Direction.Sub, b))
    val result = outcomes(constraints)
    assert(result.forall(_ == "rejected"))

  test("complementary upper bounds cover Top in every constraint order"):
    val a = TVar(TypeVar("A"))
    val b = TVar(TypeVar("B"))
    val int = TClass(ClassVar("Int"))
    val constraints = List(
      Constraint(TTop, Direction.Sub, TJointType(JointMode.Union, a, b)),
      Constraint(a, Direction.Sub, int),
      Constraint(b, Direction.Sub, TNeg(int)))
    assert(outcomes(constraints).forall(_.startsWith("accepted:")))

  test("two integer-bounded variables cannot cover Top in any constraint order"):
    val a = TVar(TypeVar("A"))
    val b = TVar(TypeVar("B"))
    val int = TClass(ClassVar("Int"))
    val constraints = List(
      Constraint(TTop, Direction.Sub, TJointType(JointMode.Union, a, b)),
      Constraint(a, Direction.Sub, int),
      Constraint(b, Direction.Sub, int))
    assert(outcomes(constraints).forall(_ == "rejected"))

  /** Interpret the Boolean/guarded fragment over two inhabited disjoint classes. */
  private def denotation(type_ : Type, assignment: Map[TypeVar, Set[Int]]): Set[Int] = type_ match
    case TTop => Set(0, 1)
    case TBot => Set.empty
    case TVar(variable) => assignment(variable)
    case TClass(ClassVar("Int")) => Set(0)
    case TClass(ClassVar("Str")) => Set(1)
    case TNeg(body) => Set(0, 1) -- denotation(body, assignment)
    case TJointType(JointMode.Inter, left, right) =>
      denotation(left, assignment) intersect denotation(right, assignment)
    case TJointType(JointMode.Union, left, right) =>
      denotation(left, assignment) union denotation(right, assignment)
    case TConstrained(body, guard) =>
      if satisfies(guard, assignment) then denotation(body, assignment) else Set(0, 1)
    case other => fail(s"Outside the finite Boolean test model: $other")

  /** A guard denotes a global subtype relation, not pointwise membership at one value. */
  private def satisfies(guard: Constraint, assignment: Map[TypeVar, Set[Int]]): Boolean =
    val left = denotation(guard.left, assignment)
    val right = denotation(guard.right, assignment)
    if guard.dir == Direction.Sub then left.subsetOf(right) else right.subsetOf(left)

  test("output retains a concrete member of the original solution family"):
    val a = TypeVar("A")
    val b = TypeVar("B")
    val original = Constraint(TJointType(JointMode.Inter, TVar(a), TVar(b)), Direction.Sub, TBot)
    val assignment = Map(a -> Set(0), b -> Set(1))
    val outs = subtypeConstraint(original)(using context)
    val guards = outs.elems.collect { case bound: Bound if bound.kind == BoundKind.Asserted =>
      bound.toConstraint }
    assert(satisfies(original, assignment))
    val retained = guards.forall(satisfies(_, assignment))
    assert(retained)

  test("isolating either polarity preserves every valuation of arbitrary opaque operands"):
    val a = TVar(TypeVar("A"))
    val b = TVar(TypeVar("B"))
    val atoms: Vector[Type] = Vector(a, b, TField("tail", a), TLam(a, b),
      TUniv(TypeVar("Q"), a), TConstrained(a, Constraint(b, Direction.Sub, a)))
    val leaves = atoms ++ Vector(TTop, TBot)
    val formulas = leaves ++ leaves.map(TNeg(_)) ++ (for
      mode <- JointMode.values.toVector
      left <- leaves
      right <- leaves
    yield TJointType(mode, left, right))
    /** Treat structural and guarded types as opaque sets at one point. */
    def member(type_ : Type, bits: Int): Boolean = type_ match
      case TTop => true
      case TBot => false
      case TNeg(body) => !member(body, bits)
      case TJointType(JointMode.Inter, left, right) => member(left, bits) && member(right, bits)
      case TJointType(JointMode.Union, left, right) => member(left, bits) || member(right, bits)
      case atom =>
        val index = atoms.indexOf(atom)
        assert(index >= 0, s"Unknown opaque atom: $atom")
        (bits & (1 << index)) != 0
    for
      remainder <- formulas
      opposite <- leaves
      leftSide <- List(false, true)
      operand <- List(a, TNeg(a))
    do
      val (sub, sup) = if leftSide then
        (TJointType(JointMode.Inter, operand, remainder), opposite)
      else (opposite, TJointType(JointMode.Union, operand, remainder))
      val constraint = isolateFlexibleClause(sub, sup)(using context).get
      for bits <- 0 until (1 << atoms.size) do
        val original = !member(sub, bits) || member(sup, bits)
        val lhs = member(constraint.left, bits)
        val rhs = member(constraint.right, bits)
        val rewritten = if constraint.dir == Direction.Sub then !lhs || rhs else !rhs || lhs
        assert(original == rewritten, s"$sub <= $sup; $constraint; bits=$bits")

  test("variables below structural types and binders are never exposed"):
    val a = TVar(TypeVar("A"))
    val b = TVar(TypeVar("B"))
    val opaque = List(TField("tail", a), TTuple(a, b), TLam(a, b), TApp(a, b),
      TUniv(TypeVar("Q"), a), TConstrained(a, Constraint(b, Direction.Sub, a)))
    for type_ <- opaque do
      assert(isolateFlexibleClause(type_, TBot)(using context).isEmpty)
      assert(isolateFlexibleClause(TTop, type_)(using context).isEmpty)
    val rigid = context.copy(clauses = context.clauses.map:
      case d: TypeVarDecl => d.copy(kind = TypeVarKind.Rigid)
      case other => other)
    assert(isolateFlexibleClause(TJointType(JointMode.Inter, a, b), TBot)(using rigid).isEmpty)

  test("operand selection respects the active declaration when kinds are shadowed"):
    val a = TypeVar("A")
    val source = TJointType(JointMode.Inter, TVar(a), TClass(ClassVar("Int")))
    val rigidShadow = context.extend(TypeVarDecl(a, TypeVarKind.Rigid, None, 1))
    assert(isolateFlexibleClause(source, TBot)(using rigidShadow).isEmpty)
    val boundedRigid = rigidShadow.extend(Bound(a, Direction.Sub,
      TClass(ClassVar("Str")), BoundKind.Asserted))
    subtype(source, TClass(ClassVar("Str")))(using boundedRigid)
    val flexShadow = rigidShadow.extend(TypeVarDecl(a, TypeVarKind.Flex, None, 2))
    assert(isolateFlexibleClause(source, TBot)(using flexShadow).exists(_.left == TVar(a)))

  test("the highest-level declaration takes priority over a later lower-level declaration"):
    val a = TypeVar("A")
    val b = TypeVar("B")
    val scoped = SubContext.empty.extend(TypeVarDecl(a, TypeVarKind.Flex, None, 2))
      .extend(TypeVarDecl(b, TypeVarKind.Flex, None, 1))
    val isolated = isolateFlexibleClause(TJointType(JointMode.Inter, TVar(a), TVar(b)), TBot)
      (using scoped).get
    assert(isolated.left == TVar(a))

  test("right-universal assumptions cannot escape as an arbitrary chosen witness"):
    val a = TypeVar("A")
    val q = TypeVar("Q")
    val target = TUniv(q, TJointType(JointMode.Union, TVar(q), TVar(a)))
    val outs = subtype(TTop, target)(using context)
    intercept[TypeError]:
      subtype(TVar(a), TClass(ClassVar("Int")))(using context.extend(outs))
    subtype(TVar(a), TTop)(using context.extend(outs))

  test("small returned solution families equal the original finite Boolean relation"):
    val subsets = Vector(Set.empty[Int], Set(0), Set(1), Set(0, 1))
    for size <- 2 to 4; mode <- JointMode.values do
      val variables = (0 until size).map(i => TypeVar(s"V$i")).toList
      val ctx = variables.foldLeft(SubContext.empty)((ctx, v) => ctx.declTypeVar(v, TypeVarKind.Flex))
      val source = variables.map(v => TVar(v): Type).reduce((a, b) => TJointType(mode, a, b))
      val original = if mode == JointMode.Inter then Constraint(source, Direction.Sub, TBot)
        else Constraint(TTop, Direction.Sub, source)
      val outs = subtypeConstraint(original)(using ctx)
      val guards = outs.elems.collect { case bound: Bound if bound.kind == BoundKind.Asserted =>
        bound.toConstraint }
      for code <- 0 until (1 << (2 * size)) do
        val assignment = variables.zipWithIndex.map((v, i) => v -> subsets((code >> (2 * i)) & 3)).toMap
        val expected = satisfies(original, assignment)
        val actual = guards.forall(satisfies(_, assignment))
        assert(expected == actual, s"$original; $guards; $assignment")
