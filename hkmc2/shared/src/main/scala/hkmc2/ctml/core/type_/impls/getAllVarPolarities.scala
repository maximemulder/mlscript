package hkmc2.ctml.core.type_.impls

import scala.collection.mutable.Set as MutSet

import hkmc2.ctml.core.debug.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*
import hkmc2.ctml.util.given
import hkmc2.ctml.core.clauses.varBound

extension (type_ : Type)
  /** Get the polarities at which a type variable occurs in the type and its variable bounds. */
  def getAllVarPolarities(var_ : TypeVar)(using ctx: Context): Polarities =
    GetAllVarPolarities1(type_, GetAllVarPolaritiesParams(var_, Polarity.Positive, ctx, MutSet()))

/** Parameters of the "get all type variable polarities"" operation. */
private class GetAllVarPolaritiesParams(val var_ : TypeVar, val pol: Polarity, val ctx: Context, val cache: MutSet[(Polarity, TypeVar)]) extends WithPolarity[GetAllVarPolaritiesParams], WithTypeVar[GetAllVarPolaritiesParams]:
  def getTypeVar: TypeVar = var_

  def setTypeVar(var_ : TypeVar): GetAllVarPolaritiesParams = GetAllVarPolaritiesParams(var_, pol, ctx, cache)

  def getPolarity = pol

  def setPolarity(pol: Polarity) = GetAllVarPolaritiesParams(var_, pol, ctx, cache)

/** Shadowing node of the "get all type variable polarities" operation. */
private object GetAllVarPolarities1 extends TypeShadowApplicator[Const[Polarities], GetAllVarPolaritiesParams](GetAllVarPolarities2):
  override def univ(univ: TUniv): Const[Polarities][Type] =
    Polarities.empty

/** Get polarity node of the "get all type variable polarities" operation. */
private object GetAllVarPolarities2 extends TypeChainApplicator[Const[Polarities], GetAllVarPolaritiesParams](GetAllVarPolarities3):
  override def apply(type_ : Type, params: GetAllVarPolaritiesParams)(using first: TypeApplicator[Const[Polarities], GetAllVarPolaritiesParams]): Polarities =
    type_ match
      case TVar(var_) if var_ == params.var_ =>
        Polarities.fromPolarity(params.pol)
      case TVar(var_) =>
        if params.cache.contains((params.pol, var_)) then
          return Polarities.empty
        else
          params.cache.add((params.pol, var_))
          params.ctx.varBound(var_, params.pol.dir) match
            case Some(boundType) =>
              GetAllVarPolarities1(boundType, params)
            case None =>
              Polarities.empty
      case _ =>
        next.apply(type_, params)

/** Polarity node of the "get all type variable polarities" operation. */
private object GetAllVarPolarities3 extends TypePolarityApplicator[Const[Polarities], Const[Polarities], GetAllVarPolaritiesParams](GetAllVarPolarities4):
  override def bound(bound: Bound, params: GetAllVarPolaritiesParams): Polarities =
    val varPolarities = if bound.var_ == params.var_
      then Polarities.fromPolarity(params.pol)
      else Polarities.empty
    val pol = bound.dir match
      case Direction.Sub =>
        params.getPolarity.invert()
      case Direction.Super =>
        params.getPolarity
    val typePolarities = GetAllVarPolarities1.apply(bound.type_, params.setPolarity(pol))
    Polarities.join(varPolarities, typePolarities)

/** Dispatching node of the "get all type variable polarities" operation. */
private object GetAllVarPolarities4 extends TypeDispatcher[Const[Polarities], Const[Polarities], GetAllVarPolaritiesParams](TypeMonoidCombinator(JoinPolaritiesMonoid)):
  override def apply(bound: Bound, params: GetAllVarPolaritiesParams): Polarities =
    val varPolarities = if bound.var_ == params.var_
      then Polarities.fromPolarity(params.pol)
      else Polarities.empty
    val pol = bound.dir match
      case Direction.Sub =>
        params.getPolarity.invert()
      case Direction.Super =>
        params.getPolarity
    val typePolarities = GetAllVarPolarities1.apply(bound.type_, params.setPolarity(pol))
    Polarities.join(varPolarities, typePolarities)
