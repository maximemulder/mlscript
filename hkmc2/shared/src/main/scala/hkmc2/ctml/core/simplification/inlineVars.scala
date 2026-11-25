package hkmc2.ctml.core.simplification

import hkmc2.ctml.config.debugInlineVar
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.type_.impls.getAllVarPolarities.*
import hkmc2.ctml.core.type_.impls.getVarPolarities.getVarPolarities
import hkmc2.ctml.core.validation.validateInferenceState
import hkmc2.ctml.types.*

/** Variables whose dependency on an enclosing level must survive simplification. */
case class NoInlineVars(vars: Set[TypeVar]):
  /** Check whether a variable must be preserved. */
  def contains(var_ : TypeVar): Boolean =
    vars.contains(var_)

/** The default policy permits inlining every variable. */
object NoInlineVars:
  /** Allow inlining when no variables have been explicitly protected. */
  given empty: NoInlineVars =
    NoInlineVars(Set.empty)

extension (type_ : Type)
  /** Determine the inlining polarities of a variable whose guard dependencies have been checked.
   *  Follow the effective bounds reachable from the type. Mixed polarities require equal bounds. */
  private[simplification] def getInlinePolarities(var_ : TypeVar)(using ctx: SubContext): Option[Polarities] =
    val polarities = type_.getAllVarPolarities(var_)

    if polarities == Polarities(false, false) then
      return Some(polarities)

    if polarities == Polarities(false, true) then
      if var_.isIndirectRecursive(Polarity.Positive) then
        return None

      return Some(polarities)

    if polarities == Polarities(true, false) then
      if var_.isIndirectRecursive(Polarity.Negative) then
        return None

      return Some(polarities)

    if checkEqual(var_.lowerBound, var_.upperBound) && !var_.isIndirectRecursive(Polarity.Negative) then
      return Some(polarities)

    None

/** Try ordinary polarity inlining in an open result. The caller chooses a local, unprotected
 *  variable declared in the output clauses; those clauses must have summarized effective bounds.
 *  The result type determines which bounds to keep, while all surviving clause occurrences must
 *  admit a non-recursive replacement. */
private[simplification] def tryInlineVar(
  type_ : Type,
  var_ : TypeVar,
  outs: SubClauses,
)(using ctx: SubContext): Option[(Type, SubClauses)] =
  assert(outs.hasVar(var_))
  assert(!outs.hasEffectiveBounds)
  val fullCtx = ctx.extend(outs)
  if !canInlineOpenState(type_, outs, var_)(using fullCtx) then
    return None
  type_.getInlinePolarities(var_)(using fullCtx).map((polarities) =>
    val (newType, newOuts) = debugInlineVar(inlineVar)(type_, var_, polarities, outs)
    validateInferenceState(s"inlining ${var_}", newType, newOuts)
    (newType, newOuts)
  )

/** Inline using only the output bounds selected by the result type's reachable polarities. */
private def inlineVar(type_ : Type, var_ : TypeVar, polarities: Polarities, outs: SubClauses)(using ctx: SubContext): (Type, SubClauses) =
  val relevantOuts = outs.filterBounds((bound) =>
    bound.var_ != var_ || polarities.contains(bound.dir.leftPol)
  )
  given SubContext = ctx.extend(relevantOuts)
  (
    type_.inline(var_),
    outs.mapBounds(_.inline(var_)).removeTypeVar(var_),
  )

/** Check that every surviving occurrence of a variable can be replaced without retaining a
  * recursive occurrence of that variable in the selected effective bound. */
private def canInlineOpenState(type_ : Type, outs: SubClauses, var_ : TypeVar)(using ctx: SubContext): Boolean =
  val typePolarities = type_.getVarPolarities(var_)
  val boundPolarities = outs.assertedBounds.iterator
    .filterNot(_.var_ == var_)
    .map((bound) =>
      val polarities = bound.type_.getVarPolarities(var_)
      if bound.dir.leftPol == Polarity.Positive then polarities else polarities.invert
    )
    .foldLeft(Polarities.empty)(Polarities.join)
  val polarities = Polarities.join(typePolarities, boundPolarities)

  (!polarities.negative || !var_.isIndirectRecursive(Polarity.Negative)) &&
    (!polarities.positive || !var_.isIndirectRecursive(Polarity.Positive))

/** Free variables related to a quantified variable by a guard. Function construction can move
 *  the quantifier outside an arrow while the guard stays in its result. The direction-based
 *  polarities of the guard alone cannot justify eliminating those free variables. */
private[simplification] def nestedGuardDependencies(type_ : Type): Set[TypeVar] =
  val dependencies = Set.newBuilder[TypeVar]

  /** Collect the free variables of guards that mention an enclosing binder. */
  def visit(type_ : Type, binders: Set[TypeVar]): Unit = type_ match
    case TUniv(bound, body) => visit(body, binders + bound)
    case TConstrained(_, guard) if binders.nonEmpty =>
      val refs = guard.getVars
      if refs.exists(binders.contains) then dependencies ++= refs.diff(binders)
      type_.components.foreach(visit(_, binders))
    case _ => type_.components.foreach(visit(_, binders))

  visit(type_, Set.empty)
  dependencies.result()

extension (outs: SubClauses)
  /** A negative occurrence uses its upper bound when inlined. For example, in `K → R` with
   *  `K ≤ Unit → List[A]`, the input's guard already depends on `A`. Protecting it here allows
   *  the simplifier to process `K` and `A` in either order. */
  private[simplification] def inputGuardDependencies(type_ : Type): Set[TypeVar] =
    val inputs = type_.getDeps(Polarity.Positive).all.collect:
      case PolarVar(variable, Polarity.Negative) => variable
    outs.assertedBounds.iterator
      .filter(bound => bound.dir == Direction.Sub && inputs.contains(bound.var_))
      .flatMap(bound => nestedGuardDependencies(bound.type_))
      .toSet
