package hkmc2.ctml.core.simplification

import scala.annotation.tailrec

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.inference.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.type_.impls.*
import hkmc2.ctml.core.type_.impls.getVarPolarities.getVarPolarities
import hkmc2.ctml.core.validation.validateInferenceState
import hkmc2.ctml.types.*

extension (ctx: SubContext)
  @tailrec
  def simplifyClauses(type_ : Type, level: Int, outs: SubClauses)(using noInlineVars: NoInlineVars): (Type, SubClauses) =
    val typeVars = type_
      .getDeps(Polarity.Positive).all
      .flatMap((dep) => Iterator.single(dep).concat(dep.var_.getTransDeps(dep.pol)(using ctx.extend(outs)).all))
      .map(_.var_)

    val inlinings = outs.typeVars
      .filterNot(noInlineVars.contains)
      .filter(ctx.extend(outs).getTypeVarEffectiveLevel(_) >= level)
      .filter(canInlineOpenState(type_, outs, _)(using ctx.extend(outs)))
      .map((var_) => type_.getInlinePolarities(var_)(using ctx.extend(outs)).map((var_, _)))
      .flatten

    inlinings.lastOption match
      case None =>
        (type_, outs)
      case Some((var_, polarities)) =>
        val (newType, newOuts) = inlineVar(type_, var_, polarities, outs)(using ctx)
        validateInferenceState(s"inlining ${var_}", newType, newOuts)(using ctx)
        ctx.simplifyClauses(newType, level, newOuts)

/** Check that every surviving occurrence of a variable can be replaced without retaining a
  * recursive occurrence of that variable in the selected effective bound. */
private def canInlineOpenState(type_ : Type, outs: SubClauses, var_ : TypeVar)(using ctx: SubContext): Boolean =
  val typePolarities = type_.getVarPolarities(var_)
  val boundPolarities = outs.bounds.iterator
    .filterNot(_.var_ == var_)
    .map((bound) =>
      val polarities = bound.type_.getVarPolarities(var_)
      if bound.dir.leftPol == Polarity.Positive then polarities else polarities.invert
    )
    .foldLeft(Polarities.empty)(Polarities.join)
  val polarities = Polarities.join(typePolarities, boundPolarities)

  (!polarities.negative || !var_.isIndirectRecursive(Polarity.Negative)) &&
    (!polarities.positive || !var_.isIndirectRecursive(Polarity.Positive))

private def hasNoLocalDeps(var_ : TypeVar, localVars: Set[TypeVar])(using ctx: SubContext): Boolean =
  Iterator(Polarity.Negative, Polarity.Positive)
    .flatMap(var_.getDeps(_).all)
    .forall((dep) => !localVars.contains(dep.var_))

private def canEliminateDisconnectedVar(var_ : TypeVar)(using ctx: SubContext): Boolean =
  val lower = var_.lowerBound
  val upper = var_.upperBound

  if lower == TBot || upper == TTop || lower == upper then
    true
  else if lower.containsVar(var_) || upper.containsVar(var_) then
    false
  else
    val comparisonCtx = ctx
      .map(_.filter(_ match
        case Bound(boundVar, _, _) => boundVar != var_
        case _ => true
      ))
      .mapCache(_ => SubtypingCache())
    checkEqual(lower, upper)(using comparisonCtx)
