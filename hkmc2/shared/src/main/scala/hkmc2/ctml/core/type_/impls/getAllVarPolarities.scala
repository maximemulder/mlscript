package hkmc2.ctml.core.type_.impls.getAllVarPolarities

import hkmc2.ctml.util.OrderedSet as MutSet

import hkmc2.ctml.core.config.*
import hkmc2.ctml.core.type_.*
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
private class GetAllVarPolaritiesParams(val var_ : TypeVar, val pol: Polarity, val ctx: Context, val cache: MutSet[(Polarity, TypeVar)]) extends PolarityParams[GetAllVarPolaritiesParams], TypeVarParams[GetAllVarPolaritiesParams]:
  override def setVar(var_ : TypeVar): GetAllVarPolaritiesParams = GetAllVarPolaritiesParams(var_, pol, ctx, cache)
  override def setPolarity(pol: Polarity) = GetAllVarPolaritiesParams(var_, pol, ctx, cache)

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
private def GetAllVarPolarities3 = TypePolarityApplicator[Const[Polarities], Const[Polarities], GetAllVarPolaritiesParams](GetAllVarPolarities4, Combinator)

/** Dispatching node of the "get all type variable polarities" operation. */
private def GetAllVarPolarities4 = TypeDispatcher[Const[Polarities], Const[Polarities], GetAllVarPolaritiesParams](Combinator)

/** Combinator node of the "get all type variable polarities" operation. */
private def Combinator = TypeMonoidCombinator[Polarities, GetAllVarPolaritiesParams](JoinPolaritiesMonoid)
