package hkmc2.ctml.core.simplification

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.type_.impls.containsVar
import hkmc2.ctml.core.type_.impls.getVarPolarities.getVarPolarities
import hkmc2.ctml.core.validation.validateInferenceState
import hkmc2.ctml.types.*

/** Try to eliminate a local variable, recovering bounds recorded on other variables first.
 *  The caller chooses an unprotected variable declared in the output clauses; those clauses must
 *  have summarized effective bounds. Analysis and rewriting share the same state, so the recovered
 *  bounds cannot be omitted from the replacement context. */
private[simplification] def tryEliminateVar(
  type_ : Type,
  var_ : TypeVar,
  outs: SubClauses,
)(using ctx: SubContext): Option[(Type, SubClauses)] =
  assert(outs.hasVar(var_))
  assert(!outs.hasEffectiveBounds)
  getEliminationBounds(type_, var_, outs)(using ctx.extend(outs)).map((flipped) =>
    val (newType, newOuts) = ctx.eliminateVar(type_, var_, outs.concat(SubClauses(flipped)))
    validateInferenceState(s"eliminating ${var_}", newType, newOuts)
    (newType, newOuts)
  )

extension (ctx: SubContext)
  /** Eliminate a variable from a type and its clauses by replacing it by its lower bound at its
   *  positive occurrences and by its upper bound at its negative occurrences, and by removing its
   *  bounds. The clauses include the additional bounds selected by `getEliminationBounds`. */
  private def eliminateVar(
    type_ : Type,
    var_ : TypeVar,
    outs: SubClauses,
  ): (Type, SubClauses) =
    // The variable is replaced in its own bounds first, so that the other bounds are inlined with
    // bounds that do not mention it.
    val selfOuts = outs.mapBounds((bound) =>
      if bound.var_ == var_ then bound.inline(var_)(using ctx.extend(outs)) else bound
    )
    val fullCtx = ctx.extend(selfOuts)
    val result = (
      type_.inline(var_)(using fullCtx),
      selfOuts.mapBounds(_.inline(var_)(using fullCtx)).removeTypeVar(var_),
    )
    assert(!result._1.containsVar(var_))
    assert(!result._2.assertedBounds.exists(_.type_.containsVar(var_)))
    result

/** Check whether a local variable can be eliminated from a type and its clauses, and get the
 *  bounds read from its direct occurrences in the bounds of the other variables.
 *
 *  A direct occurrence of the variable in the bound of another variable is a bound between the two
 *  variables, which the solver records on the other variable (see `subtypeFlexVars`): `β ≥ γ ∨ …`
 *  bounds `γ` by `β` from above, and `β ≤ γ ∧ …` from below. These occurrences are thus read as
 *  bounds of the variable rather than as occurrences of it: the result variable `γ` of a nested
 *  match is bounded from above by the result variable `β` of the enclosing match it flows into.
 *
 *  The variable can then be eliminated when its other occurrences are all positive or all
 *  negative, as at a level (see `getInlinePolarities`). It is replaced by its bound in the
 *  direction of these occurrences, which is combined with the bounds read from its direct
 *  occurrences in that direction, and its direct occurrences in the other direction are replaced
 *  by its bound in that other direction, which keeps the constraints between its bounds: e.g. `γ`
 *  is replaced by `β` in `{γ ≥ τ, β ≥ γ, α ≤ ¬({γ ≥ Str} ⟹ ¬Str)}`, which becomes
 *  `{β ≥ τ, α ≤ ¬({β ≥ Str} ⟹ ¬Str)}`.
 *
 *  Occurrences in the result type also determine the direction of elimination: a parameter `α`
 *  in `α → γ` with `β ≥ α` is negative, so its replacement includes the upper bound `β`. When
 *  `α` and `β` share the same other upper bounds, this identifies the two parameters without
 *  duplicating the constraints of an intervening lambda application.
 *
 *  The bound that replaces the variable must not mention it, and its other bound must not mention
 *  it at the polarity of the occurrences that this other bound replaces, since the variable would
 *  otherwise remain in the clauses. */
private def getEliminationBounds(
  type_ : Type,
  var_ : TypeVar,
  outs: SubClauses,
)(using ctx: SubContext): Option[List[Bound]] =
  var nested = type_.getVarPolarities(var_)
  var lowerVars = List[TypeVar]()
  var upperVars = List[TypeVar]()
  for bound <- outs.assertedBounds if bound.var_ != var_ do
    val pol = bound.dir.leftPol
    val stripped = bound.type_.removeDirectVar(var_, pol)
    nested = Polarities.join(nested, stripped.getVarPolarities(var_, pol))
    if stripped != bound.type_ then
      bound.dir match
        case Direction.Sub   => lowerVars ::= bound.var_
        case Direction.Super => upperVars ::= bound.var_
  if nested.positive && nested.negative then
    return None
  // The variable is replaced by its lower bound unless it occurs negatively, so that no bound is
  // read from its direct occurrences in the common case of a variable that only flows into others.
  val dir = if nested.negative then Direction.Sub else Direction.Super
  val flippedVars = dir match
    case Direction.Sub   => upperVars
    case Direction.Super => lowerVars
  val flipped = flippedVars.map((other) => Bound(var_, dir, TVar(other), BoundKind.Asserted))
  given SubContext = ctx.extend(SubClauses(flipped))
  val pol = dir.leftPol
  val bound = var_.bound(dir).removeDirectVar(var_, pol)
  val otherBound = var_.bound(!dir).removeDirectVar(var_, !pol)
  if bound.containsVar(var_) || otherBound.getVarPolarities(var_, !pol).contains(!pol) then
    return None
  Some(flipped)
