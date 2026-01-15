package hkmc2.ctml.core.type_.impls.getVarPolarities

import hkmc2.ctml.core.config.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*
import hkmc2.ctml.util.given

extension (type_ : Type)
  /** Get the polarities at which a type variable occurs in the type. */
  def getVarPolarities(var_ : TypeVar): Polarities =
    GetVarPolarities1(type_, GetVarPolaritiesParams(var_, Polarity.Positive))

/** Parameters of the get type variable polarities operation. */
private class GetVarPolaritiesParams(val var_ : TypeVar, val pol: Polarity) extends PolarityParams[GetVarPolaritiesParams], TypeVarParams[GetVarPolaritiesParams]:
  override def setVar(var_ : TypeVar) = GetVarPolaritiesParams(var_, pol)
  override def setPolarity(pol: Polarity) = GetVarPolaritiesParams(var_, pol)

/** Shadowing node of the "get type variable polarities" operation. */
private object GetVarPolarities1 extends TypeShadowApplicator[Const[Polarities], GetVarPolaritiesParams](GetVarPolarities2):
  override def univ(univ: TUniv): Const[Polarities][Type] =
    Polarities.empty

/** Get polarity node of the "get type variable polarities" operation. */
private object GetVarPolarities2 extends TypeChainApplicator[Const[Polarities], GetVarPolaritiesParams](GetVarPolarities3):
  override def apply(type_ : Type, params: GetVarPolaritiesParams)(using first: TypeApplicator[Const[Polarities], GetVarPolaritiesParams]): Polarities =
    type_ match
      case TVar(var_) if var_ == params.var_ =>
        Polarities.fromPolarity(params.pol)
      case _ =>
        next.apply(type_, params)

/** Polarity node of the "get type variable polarities" operation. */
private def GetVarPolarities3 = TypePolarityApplicator[Const[Polarities], Const[Polarities], GetVarPolaritiesParams](GetVarPolarities4, Combinator)

/** Dispatching node of the "get type variable polarities" operation. */
private def GetVarPolarities4 = TypeDispatcher[Const[Polarities], Const[Polarities], GetVarPolaritiesParams](Combinator)

/** Combinator node of the "get type variable polarities" operation. */
private def Combinator = TypeMonoidCombinator[Polarities, GetVarPolaritiesParams](JoinPolaritiesMonoid)
