package hkmc2.ctml

import org.scalatest.funsuite.AnyFunSuite

import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.types.*

/** The abstract-cut exception recognizes literal progress independently of AST allocation. */
class SyntaxDescentTest extends AnyFunSuite:
  /** A fixed ambient variable, compared by its concrete identity. */
  private val carrier: Type = TVar(TypeVar("Carrier"))

  /** A compact, intentionally unshared oracle for small syntax only. */
  private def subterms(type_ : Type): List[Type] =
    type_ :: type_.components.flatMap(subterms)

  /** The specified product order, evaluated without the production DAG representation. */
  private def reference(current: (Type, Type), previous: (Type, Type)): Boolean =
    current != previous && subterms(previous._1).contains(current._1) &&
      subterms(previous._2).contains(current._2)

  test("small heterogeneous syntax agrees with the direct subterm oracle"):
    val a = TVar(TypeVar("A"))
    val b = TVar(TypeVar("B"))
    val int = TClass(ClassVar("Int"))
    val str = TClass(ClassVar("Str"))
    val atoms = List(TBot, TTop, a, b, int, str)
    val compounds = List(TNeg(a), TField("x", a), TField("y", a), TTuple(a, b),
      TLam(a, b), TApp(a, b), TJointType(JointMode.Union, a, b),
      TJointType(JointMode.Inter, a, b), TUniv(TypeVar("A"), TLam(a, a)),
      TUniv(TypeVar("B"), TLam(b, b)),
      TConstrained(a, Constraint(a, Direction.Sub, b)),
      TConstrained(a, Constraint(a, Direction.Super, b)))
    val types = atoms ++ compounds ++ compounds.map(TField("outer", _))
    val oldPairs = types.map(_ -> carrier) ++ types.map(carrier -> _) ++
      compounds.map(type_ => (TField("left", type_), TField("right", type_)))
    val newPairs = types.map(_ -> carrier) ++ types.map(carrier -> _) ++
      types.map(type_ => (type_, type_))
    for previous <- oldPairs; current <- newPairs do
      assert(syntaxDescends(current, previous) == reference(current, previous))

  test("both sides may descend but neither may grow"):
    val a = TVar(TypeVar("A"))
    val b = TVar(TypeVar("B"))
    val old = (TField("left", a), TField("right", b))
    assert(syntaxDescends((a, b), old))
    assert(!syntaxDescends((a, TField("larger", old._2)), old))
    assert(!syntaxDescends((TBot, b), old))

  test("latest representatives prevent alternation among incomparable descendants"):
    val a = TField("a", TTop)
    val b = TField("b", TBot)
    val root = TTuple(a, b)
    assert(syntaxDescends((a, carrier), (root, carrier)))
    assert(syntaxDescends((b, carrier), (root, carrier)))
    assert(!syntaxDescends((b, carrier), (a, carrier)))
    assert(!syntaxDescends((root, carrier), (a, carrier)))

  test("same provenance does not identify distinct variables or alpha-renamed binders"):
    val origin = VarProvenance(VarOrigin.Named("shared"), VarRole.Open(TypeVarKind.Flex), Set.empty)
    val a = TVar(new TypeVar("A", origin))
    val b = TVar(new TypeVar("B", origin))
    assert(!syntaxDescends((b, carrier), (TField("x", a), carrier)))
    assert(!syntaxDescends((a, b), (TField("x", a), a)))
    val first = TUniv(TypeVar("A"), TLam(a, a))
    val renamed = TUniv(TypeVar("B"), TLam(b, b))
    assert(!syntaxDescends((renamed, carrier), (TField("x", first), carrier)))
    // Different metadata on the same concrete name agrees with TypeVar.equals.
    assert(syntaxDescends((TVar(TypeVar("A")), carrier), (TField("x", a), carrier)))

  test("field names, class names, joint modes, and guard directions are exact"):
    val int = TClass(ClassVar("Int"))
    val str = TClass(ClassVar("Str"))
    val upper = TConstrained(int, Constraint(int, Direction.Sub, str))
    val lower = TConstrained(int, Constraint(int, Direction.Super, str))
    assert(!syntaxDescends((lower, carrier), (TField("x", upper), carrier)))
    assert(!syntaxDescends((TField("y", int), carrier),
      (TField("outer", TField("x", int)), carrier)))
    assert(!syntaxDescends((str, carrier), (TField("x", int), carrier)))
    assert(!syntaxDescends((TJointType(JointMode.Union, int, str), carrier),
      (TField("x", TJointType(JointMode.Inter, int, str)), carrier)))

  test("copied and shared subtrees have the same progress decision"):
    val shared = TTuple(TField("x", TTop), TField("y", TBot))
    val copied = TTuple(TField("x", TTop), TField("y", TBot))
    val parent = TField("outer", shared)
    assert(syntaxDescends((shared, carrier), (parent, carrier)))
    assert(syntaxDescends((copied, TVar(TypeVar("Carrier"))), (parent, carrier)))
    assert(!syntaxDescends((copied, carrier), (shared, carrier)))

  test("deep doubled-arrow DAGs do not unfold during equality or subterm checks"):
    /** Allocate linear DAGs whose unfolded syntax has exponentially many occurrences. */
    def doubled(depth: Int): Type =
      (0 until depth).foldLeft[Type](TClass(ClassVar("Leaf")))((body, _) => TLam(body, body))
    for depth <- List(512, 2048, 8192) do
      val first = doubled(depth)
      val copied = doubled(depth)
      assert(syntaxDescends((copied, carrier), (TField("outer", first), carrier)))
      assert(!syntaxDescends((copied, carrier), (first, carrier)))
      assert(!syntaxDescends((TField("outer", copied), carrier), (first, carrier)))
