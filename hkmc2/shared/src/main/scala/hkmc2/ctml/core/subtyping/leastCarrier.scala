package hkmc2.ctml.core.subtyping

import scala.annotation.tailrec
import scala.collection.mutable

import hkmc2.ctml.core.structural.{getVars, hasVar, removeDirectVar, structuralCombine}
import hkmc2.ctml.core.type_.impls.getVarPolarities.getVarPolarities
import hkmc2.ctml.core.type_.impls.substitute.substitute
import hkmc2.ctml.core.var_.newFreshVar
import hkmc2.ctml.types.*
import hkmc2.utils.SccAnalysis

/** Lower inequalities of a single coordinate. At the least tuple its value is their union. */
private case class Carrier(variable: TypeVar, lowers: List[Type]):
  assert(lowers.nonEmpty)

/** Instantiate cyclic coordinates of a covariant producer at their simultaneous least solution.
 *  For fixed values of the other parameters, each defining inequality has the form L(Xs) ≤ Xi.
 *  Shrinking the tuple to the least one preserves every residual guard because its lower operand
 *  can only shrink and its upper operand can only grow. Each substituted guard remains in scope.
 */
private[subtyping] def leastCarrierInstance(scheme: TUniv): Option[Type] =
  val (variables, guarded) = scheme.getUnivComponents
  val (body, guards) = guarded.getConstrainedComponents
  if guards.isEmpty then return None

  val bounds = guards.flatMap: g =>
    val (lower, upper) = if g.dir == Direction.Sub then (g.left, g.right) else (g.right, g.left)
    upper.exactComponents(JointMode.Inter).map((lower, _))
  val byOwner = bounds.collect { case (l, TVar(x)) => (x, l) }.groupMap(_._1)(_._2)

  /** Guard polarity inside nested constrained types requires a separate variance analysis. */
  def occursOnly(t: Type, x: TypeVar, pol: Polarity): Boolean =
    !t.hasVar(x) || !mentionsConstrained(t) && !t.getVarPolarities(x).contains(!pol)

  val eligible = variables.distinct.filter: x =>
    byOwner.contains(x) && occursOnly(body, x, Polarity.Positive) && bounds.forall: (l, u) =>
      occursOnly(l, x, Polarity.Positive) && (u == TVar(x) || occursOnly(u, x, Polarity.Negative))
  val candidates = eligible.toSet
  val deps = eligible.map(x => x -> byOwner(x).flatMap(_.getVars).filter(candidates)).toMap
  val cyclic = SccAnalysis.sccsFrom[TypeVar](deps, eligible)
    .filter(xs => xs.size > 1 || deps(xs.head).contains(xs.head)).flatten.toSet
  if cyclic.isEmpty then return None

  val owners: Set[Type] = cyclic.map(TVar.apply)
  val residual = bounds.filterNot((_, upper) => owners.contains(upper))
  // A unary projection is already the encoding of its least carrier.
  if cyclic.size == 1 && owners.contains(body) && residual.isEmpty then return None

  val result = residual.foldRight(body)((bound, t) =>
    TConstrained(t, Constraint(bound._1, Direction.Sub, bound._2)))
  val equations = eligible.filter(cyclic).map(x => Carrier(x, byOwner(x)))
  val prepared = if equations.size == 1 then equations else shareSubexpressions(equations)
  eliminateCarriers(prepared, result).map: instance =>
    variables.filterNot(cyclic).foldRight(instance)(TUniv.apply)

/** Give repeated positive subexpressions a single coordinate. Each definition E ≤ H is exact
 *  at the least tuple because replacing H by E preserves every covariant use. Elimination can
 *  then close a cycle at a shared expression inside a bound.
 *  Binding constructs and contravariant operands are opaque to this transformation.
 */
private def shareSubexpressions(equations: List[Carrier]): List[Carrier] =
  val names = mutable.LinkedHashMap.empty[List[Type], TypeVar]
  val owners = equations.map(_.variable).toSet

  /** Name a union once, preserving its summands as individual lower bounds. */
  def node(lowers: List[Type]): Type = TVar(names.getOrElseUpdate(lowers, newFreshVar()))

  /** Intern the covariant syntax as a directed acyclic graph of lower inequalities. */
  def name(t: Type): Type =
    if !t.getVars.exists(owners) then return t
    val expr = t match
      case TApp(f, a) => TApp(name(f), name(a))
      case TTuple(l, r) => TTuple(name(l), name(r))
      case TField(k, v) => TField(k, name(v))
      case TJointType(m, l, r) => TJointType(m, name(l), name(r))
      case TLam(p, r) => TLam(p, name(r))
      case _ => return t
    node(List(expr))

  // A union of summands used by the same owners can itself be shared, including when each
  // owner has further, private lower bounds. Preserve first occurrence order in the guards.
  val layers = equations.flatMap: eq =>
    eq.lowers.flatMap(_.exactComponents(JointMode.Union)).distinct.map((_, eq.variable))
  val targets = layers.groupMap(_._1)(_._2)
  val summands = layers.map(_._1).distinct
  val byTargets = summands.groupBy(targets)
  val groups = summands.map(targets).distinct.map: xs =>
    val parts = byTargets(xs).map(name)
    xs -> (if xs.size > 1 && parts.size > 1 then List(node(parts)) else parts)
  val roots = equations.map: eq =>
    val lowers = groups.collect { case (xs, ls) if xs.contains(eq.variable) => ls }.flatten
    Carrier(eq.variable, lowers)
  roots ++ names.iterator.map((ls, x) => Carrier(x, ls))

/** Bekić elimination. With the remaining coordinates Y fixed, the least X is
 *  W(Y) = μX.⋁L(X,Y), encoded as ∀X. {L(X,Y) ≤ X} ⇒ X. W is monotone in Y.
 *  Substituting W(Y) into the other inequalities gives a smaller system. Any original solution
 *  (X,Y) satisfies W(Y) ≤ X, hence Y solves the reduced system. Conversely every reduced solution
 *  Y extends by W(Y). The least such Y gives the least original tuple.
 */
@tailrec
private def eliminateCarriers(equations: List[Carrier], result: Type): Option[Type] =
  if !fitsCarrierBudget(result :: equations.flatMap(_.lowers)) then return None
  if equations.isEmpty then return Some(result)

  // A nonrecursive coordinate needs only a union. Prefer eliminating a coordinate used by
  // fewer other equations, so that a shared definition stays in scope until its uses are closed.
  val free = equations.map(eq => eq.variable -> eq.lowers.flatMap(_.getVars).toSet).toMap
  val uses = free.valuesIterator.flatten.toList.groupMapReduce(identity)(_ => 1)(_ + _)
  val inResult = result.getVars
  val chosen = equations.minBy: eq =>
    val x = eq.variable
    (free(x)(x), uses.getOrElse(x, 0), inResult(x))
  val x = chosen.variable
  val lowers = chosen.lowers.map(_.removeDirectVar(x, Polarity.Positive)).distinct
    .filterNot(_ == TBot)
  val witness = if lowers.exists(_.hasVar(x)) then
    val guarded = lowers.foldRight(TVar(x): Type)((l, t) =>
      TConstrained(t, Constraint(TVar(x), Direction.Super, l)))
    TUniv(x, guarded)
  else lowers.foldRight(TBot: Type)(structuralCombine(JointMode.Union, _, _))
  val rest = equations.collect:
    case eq if eq.variable != x =>
      if free(eq.variable)(x) then eq.copy(lowers = eq.lowers.map(_.substitute(x, witness))) else eq
  eliminateCarriers(rest, if inResult(x) then result.substitute(x, witness) else result)

/** Dense systems can expand exponentially as ordinary type syntax. Count unfolded nodes before
 *  traversing the next intermediate system. Ordinary universal instantiation handles schemes
 *  beyond this budget. Substitution shares the inserted witness at every use.
 */
private def fitsCarrierBudget(types: List[Type]): Boolean =
  @tailrec
  def visit(pending: List[Type], left: Int): Boolean = pending match
    case Nil => true
    case head :: tail => left > 0 && visit(head.components ::: tail, left - 1)
  visit(types, 4096)
