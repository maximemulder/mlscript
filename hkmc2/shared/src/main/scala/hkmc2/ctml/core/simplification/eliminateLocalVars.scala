package hkmc2.ctml.core.simplification

import scala.annotation.tailrec

import hkmc2.ctml.config.debug
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.type_.impls.*
import hkmc2.ctml.core.type_.impls.getVarPolarities.getVarPolarities
import hkmc2.ctml.core.type_.impls.inline.inline
import hkmc2.ctml.core.validation.validateInferenceState
import hkmc2.ctml.types.*

// Elimination of the type variables declared in the clauses of a branch, before these clauses are
// joined with those of another branch (see `joinBounds`).

// The variables declared in the clauses of a branch are local to it: the other branch is not in
// their scope, and nothing but the bounds of these clauses refers to them. The clauses thus
// quantify them existentially, and such a variable can be eliminated by replacing it by one of its
// bounds whenever this loses no solution of the clauses, as at the end of a level (see
// `simplifyClauses`). This is done before the join rather than after, since the join guards the
// bound of every variable by all the other bounds of its branch (see `joinVarBounds`): once joined,
// a local variable is nested in these guards at both polarities, and can no longer be eliminated by
// polarity. The result variable of a nested match would notably survive the join with the result
// variable of the enclosing match it flows into: e.g. `foo` in `ctmlFlow.mls` would have one more
// type variable per branch.

extension (ctx: SubContext)
  /** Eliminate the type variables declared in the clauses of a branch when this loses no solution
   *  of these clauses. Only the asserted bounds of the clauses are kept, which are the bounds that
   *  the join uses (see `BoundKind`). */
  def eliminateLocalVars(branch: SubClauses): SubClauses =
    val outs = branch.removeEffectiveBounds()
    // Variables extruded through an outer variable are declared below the current level, and are
    // kept since they stand for that outer variable (see `processLevel`).
    val localVars = outs.typeVars.filter(_.level(using ctx.extend(outs)) >= ctx.level).toSet
    ctx.eliminateVars(localVars, outs)

  /** Eliminate the given local variables of the clauses one at a time, as long as one of them can
   *  be eliminated. */
  @tailrec
  private def eliminateVars(vars: Set[TypeVar], outs: SubClauses): SubClauses =
    val fullCtx = ctx.extend(outs)
    val elimination = outs.typeVars
      .filter(vars.contains)
      .flatMap((var_) => getFlippedBounds(var_, outs)(using fullCtx).map((var_, _)))
      .lastOption
    elimination match
      case None =>
        outs
      case Some((var_, flipped)) =>
        val newOuts = ctx.eliminateVar(var_, outs.concat(SubClauses(flipped)))
        validateInferenceState(s"eliminating ${var_}", TTop, newOuts)(using ctx)
        debug(s"ELIMINATE ${var_} IN ${outs} OUT ${newOuts}")
        ctx.eliminateVars(vars, newOuts)

  /** Eliminate a variable from the clauses by replacing it by its lower bound at its positive
   *  occurrences and by its upper bound at its negative occurrences, and by removing its bounds. */
  private def eliminateVar(var_ : TypeVar, outs: SubClauses): SubClauses =
    // The variable is replaced in its own bounds first, so that the other bounds are inlined with
    // bounds that do not mention it.
    val selfOuts = outs.mapBounds((bound) =>
      if bound.var_ == var_ then bound.inline(var_)(using ctx.extend(outs)) else bound
    )
    selfOuts.mapBounds(_.inline(var_)(using ctx.extend(selfOuts))).removeTypeVar(var_)

/** Check whether a local variable can be eliminated from the clauses, and get the bounds of the
 *  variable that are read from its direct occurrences in the bounds of the other variables.
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
 *  The bound that replaces the variable must not mention it, and its other bound must not mention
 *  it at the polarity of the occurrences that this other bound replaces, since the variable would
 *  otherwise remain in the clauses. */
private def getFlippedBounds(
  var_ : TypeVar,
  outs: SubClauses,
)(using ctx: SubContext): Option[List[Bound]] =
  var nested = Polarities.empty
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
