package hkmc2.ctml.core.simplification

import scala.annotation.tailrec

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.inference.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.type_.impls.getVarPolarities.getVarPolarities
import hkmc2.ctml.core.validation.validateInferenceState
import hkmc2.ctml.types.*

extension (ctx: SubContext)
  /** Simplify local variables in an open inference result, including relations recorded only on
   *  another variable. Keep variables whose effective level belongs to an enclosing scope. */
  @tailrec
  def simplifyClauses(type_ : Type, level: Int, outs: SubClauses)(using noInlineVars: NoInlineVars): (Type, SubClauses) =
    val fullCtx = ctx.extend(outs)
    val vars = outs.typeVars
      .filterNot(noInlineVars.contains)
      .filter(fullCtx.getTypeVarEffectiveLevel(_) >= level)
    val inlinings = vars
      .filter(canInlineOpenState(type_, outs, _)(using fullCtx))
      .map((var_) => type_.getInlinePolarities(var_)(using fullCtx).map((var_, _)))
      .flatten

    inlinings.lastOption match
      case None =>
        // Direct occurrences in another variable's bounds may hide a bound in the direction
        // needed here: `β ≥ α` supplies `α ≤ β` when eliminating a negative parameter `α`.
        val elimination = vars
          .flatMap((var_) => getEliminationBounds(type_, var_, outs)(using fullCtx)
            .map((var_, _)))
          .lastOption
        elimination match
          case None =>
            (type_, outs)
          case Some((var_, flipped)) =>
            val (newType, newOuts) = ctx.eliminateVar(type_, var_, outs.concat(SubClauses(flipped)))
            validateInferenceState(s"eliminating ${var_}", newType, newOuts)(using ctx)
            ctx.simplifyClauses(newType, level, newOuts)
      case Some((var_, polarities)) =>
        val (newType, newOuts) = inlineVar(type_, var_, polarities, outs)(using ctx)
        validateInferenceState(s"inlining ${var_}", newType, newOuts)(using ctx)
        ctx.simplifyClauses(newType, level, newOuts)

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
