package hkmc2.ctml.core.simplification

import scala.annotation.tailrec

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.inference.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.type_.impls.*
import hkmc2.ctml.core.validation.validateInferenceState
import hkmc2.ctml.types.*

extension (ctx: SubContext)
  @tailrec
  def simplifyClauses2(type_ : Type, level: Int, outs: SubClauses)(using noInlineVars: NoInlineVars): (Type, SubClauses) =
    val typeVars = type_
      .getDeps(Polarity.Positive).all
      .flatMap((dep) => Iterator.single(dep).concat(dep.var_.getTransDeps(dep.pol)(using ctx.extend(outs)).all))
      .map(_.var_)

    val inlinings = outs.typeVars
      .filterNot(noInlineVars.contains)
      .filter(ctx.extend(outs).getTypeVarEffectiveLevel(_) >= level)
      .map((var_) => type_.getInlinePolarities(var_)(using ctx.extend(outs)).map((var_, _)))
      .flatten

    inlinings.lastOption match
      case None =>
        (type_, outs)
      case Some((var_, polarities)) =>
        val (newType, newOuts) = inlineVar(type_, var_, polarities, outs)(using ctx)
        ctx.simplifyClauses2(newType, level, newOuts)

  /** Eliminate level-local variables disconnected from an open inference result.
    *
    * Dependencies are followed polarly through effective bounds. Variables with distinct lower
    * and upper bounds are retained until clause projection can preserve their implied sandwich
    * constraint instead of silently discarding it.
    */
  def simplifyClauses(type_ : Type, level: Int, outs: SubClauses): (Type, SubClauses) =
    validateInferenceState("entering clause simplification", type_, outs)(using ctx)

    val levelCtx = ctx.extend(outs)
    val localVars = levelCtx.getLevelVars(level).filter(outs.hasVar)
    val localVarSet = localVars.toSet
    val resultDeps = type_.getDeps(Polarity.Positive).all
    val requiredVars = resultDeps.iterator
      .flatMap((dep) => Iterator.single(dep).concat(dep.var_.getTransDeps(dep.pol)(using levelCtx).all))
      .map(_.var_)
      .filter(localVarSet.contains)
      .toSet

    val action = localVars.iterator
      .filterNot(requiredVars.contains)
      .filter(levelCtx.getTypeVarEffectiveLevel(_) >= level)
      .filter(hasNoLocalDeps(_, localVarSet)(using levelCtx))
      .filter(canEliminateDisconnectedVar(_)(using levelCtx))
      .flatMap((var_) => type_.getInlinePolarities(var_)(using levelCtx).map((var_, _)))
      .nextOption()

    action match
      case None =>
        (type_, outs)
      case Some((var_, polarities)) =>
        val (nextType, nextOuts) = inlineVar(type_, var_, polarities, outs)(using levelCtx)
        validateInferenceState(s"inlining ${var_}", nextType, nextOuts)(using ctx)
        ctx.simplifyClauses(nextType, level, nextOuts)

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
