package hkmc2.ctml.core.simplification

import scala.annotation.tailrec

import hkmc2.ctml.config.debug
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
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
    val elimination = outs.typeVars.reverseIterator
      .filter(vars.contains)
      .flatMap((var_) => tryEliminateVar(TTop, var_, outs)(using ctx).map((var_, _)))
      .nextOption()
    elimination match
      case None =>
        outs
      case Some((var_, (_, newOuts))) =>
        debug(s"ELIMINATE ${var_} IN ${outs} OUT ${newOuts}")
        ctx.eliminateVars(vars, newOuts)
