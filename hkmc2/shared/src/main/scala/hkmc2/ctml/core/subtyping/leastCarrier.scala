package hkmc2.ctml.core.subtyping

import scala.annotation.tailrec
import scala.collection.mutable

import hkmc2.ctml.core.structural.{TypeSyntaxKey, getVars, hasVar, removeDirectVar, sameTypeSyntax,
  structuralCombine}
import hkmc2.ctml.core.type_.impls.substitute.substitute
import hkmc2.ctml.core.var_.newFreshVar
import hkmc2.ctml.types.*
import hkmc2.utils.SccAnalysis

/** Lower inequalities of a single coordinate. At the least tuple its value is their union. */
private case class Carrier(variable: TypeVar, lowers: List[Type]):
  assert(lowers.nonEmpty)

  /** Free variables are shared by the dependency analysis and successive elimination steps. */
  lazy val dependencies: Set[TypeVar] = lowers.iterator.flatMap(_.getVars).toSet

/** Two presentations of the same least instance. Build the alternative only on demand. */
private[subtyping] case class CarrierInstances(primary: Type, alternative: () => Option[Type])

/** Instantiate cyclic coordinates of a covariant producer at their simultaneous least solution.
 *  For fixed values of the other parameters, each defining inequality has the form L(Xs) ≤ Xi.
 *  Shrinking the tuple to the least one preserves every residual guard because its lower operand
 *  can only shrink and its upper operand can only grow. Each substituted guard remains in scope.
 */
private[subtyping] def leastCarrierInstances(scheme: TUniv): Option[CarrierInstances] =
  val (variables, guarded) = scheme.getUnivComponents
  val (body, guards) = guarded.getConstrainedComponents
  if guards.isEmpty then return None

  val bounds = guards.flatMap: g =>
    val (lower, upper) = if g.dir == Direction.Sub then (g.left, g.right) else (g.right, g.left)
    upper.exactComponents(JointMode.Inter).map:
      // In L(X) ≤ X, Boolean occurrences of X at the tested value can be set to bottom.
      // Structural occurrences still govern monotonicity and the recursive witness.
      case owner @ TVar(x) if variables.contains(x) =>
        (normalizeRecursiveBound(x, Direction.Super, lower), owner)
      case other => (lower, other)
  val byOwner = bounds.collect { case (l, TVar(x)) => (x, l) }.groupMap(_._1)(_._2)

  /** In {L ≤ U} ⇒ B, to widen the type we may strengthen its guard and widen B.
   *  Shrinking L and growing U therefore shrink the constrained type. An enclosing arrow
   *  domain or negation reverses all three operand polarities.
   */
  def occursOnly(t: Type, x: TypeVar, pol: Polarity): Boolean = t match
    case TVar(y) => x != y || pol == Polarity.Positive
    case TNeg(t) => occursOnly(t, x, !pol)
    case TLam(p, r) => occursOnly(p, x, !pol) && occursOnly(r, x, pol)
    case TUniv(y, t) => y == x || occursOnly(t, x, pol)
    case TConstrained(t, g) =>
      occursOnly(t, x, pol) && occursOnly(g.left, x, pol * g.dir.rightPol) &&
        occursOnly(g.right, x, pol * g.dir.leftPol)
    case _ => t.components.forall(occursOnly(_, x, pol))

  val carriers = variables.distinct.collect:
    case x if byOwner.contains(x) => Carrier(x, byOwner(x))
  val owners = carriers.iterator.map(_.variable).toSet
  val eligible = carriers.filter: eq =>
    val x = eq.variable
    eq.dependencies.exists(owners) && occursOnly(body, x, Polarity.Positive) &&
      bounds.forall: (l, u) =>
        occursOnly(l, x, Polarity.Positive) && (u == TVar(x) || occursOnly(u, x, Polarity.Negative))
  val components = eligible match
    case Nil => return None
    case eq :: Nil => if eq.dependencies(eq.variable) then List(List(eq.variable)) else Nil
    case _ =>
      val deps = eligible.map(eq => eq.variable -> eq.dependencies).toMap
      SccAnalysis.sccsFrom[TypeVar](x => deps(x).filter(deps.contains), eligible.map(_.variable))
        .filter(xs => xs.size > 1 || deps(xs.head)(xs.head))

  // Close dependencies first. A component whose witness exceeds the budget remains a parameter
  // of any other component, with all its original inequalities still in scope.
  components.iterator.flatMap: component =>
    val members = component.toSet
    val residual = bounds.filterNot:
      case (_, TVar(x)) => members(x)
      case _ => false
    if component.size == 1 && body == TVar(component.head) && residual.isEmpty then None
    else
      val result = residual.foldRight(body)((bound, t) =>
        TConstrained(t, Constraint(bound._1, Direction.Sub, bound._2)))
      val equations = eligible.filter(eq => members(eq.variable))
      val prepared = if equations.size == 1 then equations else shareSubexpressions(equations)
      if carrierSize(result :: prepared.flatMap(_.lowers), component.head) > carrierBudget then None
      else eliminateCarriers(prepared, result).map: instance =>
        val parameters = variables.filterNot(members)
        /** Preserve the binders of coordinates outside the selected component. */
        def close(t: Type): Type = parameters.foldRight(t)(TUniv.apply)
        // Tied elimination orders can fold the same cycle at different coordinates. An exact
        // target can match one presentation even when bounded subtyping fails on the other.
        CarrierInstances(close(instance), () =>
          if equations.size == 1 then None
          else
            val (binders, shared) = prepared.splitAt(equations.size)
            eliminateCarriers(binders.reverse ::: shared, result)
              .filterNot(sameTypeSyntax(_, instance)).map(close))
  .nextOption()

/** Give repeated positive subexpressions a single coordinate. Each definition E ≤ H is exact
 *  at the least tuple because replacing H by E preserves every covariant use. Elimination can
 *  then close a cycle at a shared expression inside a bound.
 *  Binding constructs and contravariant operands are opaque to this transformation.
 */
private def shareSubexpressions(equations: List[Carrier]): List[Carrier] =
  val names = mutable.LinkedHashMap.empty[List[TypeSyntaxKey], TypeVar]
  val owners = equations.map(_.variable).toSet

  /** Name a union once, preserving its summands as individual lower bounds. */
  def node(lowers: List[Type]): Type =
    TVar(names.getOrElseUpdate(lowers.map(TypeSyntaxKey(_)), newFreshVar()))

  // A second incoming edge justifies naming a node. Its children have already been visited
  // through the first edge, and will occur only once in the shared definition.
  val seen = mutable.HashSet.empty[TypeSyntaxKey]
  val shared = mutable.HashSet.empty[TypeSyntaxKey]

  /** Count incoming edges in the graph of covariant constructor subexpressions. */
  def visit(t: Type): Unit = t match
    case _: TApp | _: TTuple | _: TField | _: TJointType | _: TLam =>
      val key = TypeSyntaxKey(t)
      if !seen.add(key) then shared += key
      else t match
        case TLam(_, r) => visit(r)
        case _ => t.components.foreach(visit)
    case _ => ()

  /** Rebuild a layer with references to its shared, carrier-dependent subexpressions. */
  def name(t: Type): Type =
    val expr = t match
      case TApp(f, a) => TApp(name(f), name(a))
      case TTuple(l, r) => TTuple(name(l), name(r))
      case TField(k, v) => TField(k, name(v))
      case TJointType(m, l, r) => TJointType(m, name(l), name(r))
      case TLam(p, r) => TLam(p, name(r))
      case _ => return t
    if shared(TypeSyntaxKey(t)) && t.getVars.exists(owners) then node(List(expr)) else expr

  // A union of summands used by the same owners can itself be shared, including when each
  // owner has further, private lower bounds. Preserve first occurrence order in the guards.
  val layers = equations.flatMap: eq =>
    eq.lowers.flatMap(_.exactComponents(JointMode.Union)).map(TypeSyntaxKey(_)).distinct
      .map((_, eq.variable))
  layers.foreach((key, _) => visit(key.type_))
  val targets = layers.groupMap(_._1)(_._2)
  val summands = layers.map(_._1).distinct
  val byTargets = summands.groupBy(targets)
  val groups = summands.map(targets).distinct.map: xs =>
    val parts = byTargets(xs).map(key => name(key.type_))
    xs -> (if xs.size > 1 && parts.size > 1 then List(node(parts)) else parts)
  val roots = equations.map: eq =>
    val lowers = groups.collect { case (xs, ls) if xs.contains(eq.variable) => ls }.flatten
    Carrier(eq.variable, lowers)
  roots ++ names.iterator.map((ls, x) => Carrier(x, ls.map(_.type_)))

/** Bekić elimination. With the remaining coordinates Y fixed, the least X is
 *  W(Y) = μX.⋁L(X,Y), encoded as ∀X. {L(X,Y) ≤ X} ⇒ X. W is monotone in Y.
 *  Substituting W(Y) into the other inequalities gives a smaller system. Any original solution
 *  (X,Y) satisfies W(Y) ≤ X, hence Y solves the reduced system. Conversely every reduced solution
 *  Y extends by W(Y). The least such Y gives the least original tuple.
 */
@tailrec
private def eliminateCarriers(equations: List[Carrier], result: Type): Option[Type] =
  if equations.isEmpty then return Some(result)

  // A nonrecursive coordinate needs only a union. Prefer eliminating a coordinate used by
  // fewer other equations, so that a shared definition stays in scope until its uses are closed.
  val inResult = result.getVars
  val chosen = equations match
    case eq :: Nil => eq
    case _ =>
      val uses = equations.flatMap(_.dependencies).groupMapReduce(identity)(_ => 1)(_ + _)
      equations.minBy: eq =>
        val x = eq.variable
        (eq.dependencies(x), uses.getOrElse(x, 0), inResult(x))
  val x = chosen.variable
  val lowers = chosen.lowers.map(_.removeDirectVar(x, Polarity.Positive)).distinctBy(TypeSyntaxKey(_))
    .filterNot(_ == TBot)
  val witness = if lowers.exists(_.hasVar(x)) then
    val guarded = lowers.foldRight(TVar(x): Type)((l, t) =>
      TConstrained(t, Constraint(TVar(x), Direction.Super, l)))
    TUniv(x, guarded)
  else lowers.foldRight(TBot: Type)(structuralCombine(JointMode.Union, _, _))
  val others = equations.filterNot(_.variable == x)
  val nextTypes = result :: others.flatMap(_.lowers)
  val witnessSize = carrierSize(List(witness), x)
  if carrierSize(nextTypes, x, witnessSize) > carrierBudget then return None
  val rest = others.map: eq =>
    if eq.dependencies(x) then eq.copy(lowers = eq.lowers.map(_.substitute(x, witness))) else eq
  eliminateCarriers(rest, if inResult(x) then result.substitute(x, witness) else result)

/** Maximum unfolded syntax size of an intermediate carrier system. */
private val carrierBudget = 4096

/** Size after substituting a witness of the given size for free X. Stop once the budget is
 *  exceeded. Renaming a binder to avoid capture preserves its size.
 */
private def carrierSize(types: List[Type], x: TypeVar, weight: Int = 1): Int =
  /** Subtrees shadowing X are counted at unit weight after all free occurrences. */
  @tailrec
  def count(todo: List[Type], shadowed: List[Type], cost: Int, size: Int): Int =
    if size > carrierBudget then size
    else todo match
      case Nil =>
        if shadowed.isEmpty then size else count(shadowed, Nil, 1, size)
      case TVar(y) :: rest if y == x => count(rest, shadowed, cost, size + cost)
      case TUniv(y, body) :: rest if y == x && cost != 1 =>
        count(rest, body :: shadowed, cost, size + 1)
      case t :: rest => count(t.components ::: rest, shadowed, cost, size + 1)
  count(types, Nil, weight, 0)
