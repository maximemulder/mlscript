package hkmc2.ctml.core.type_.impls

import hkmc2.ctml.core.debug.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*
import hkmc2.ctml.util.given

extension (type_ : Type)
  /** Get the polarities at which a type variable occurs in the type. */
  def getVarPolarities(var_ : TypeVar): Polarities =
    GetVarPolarities1(type_, GetVarPolaritiesParams(var_, Polarity.Positive))

/** Parameters of the get type variable polarities operation. */
private class GetVarPolaritiesParams(val var_ : TypeVar, val pol: Polarity) extends WithPolarity[GetVarPolaritiesParams], WithTypeVar[GetVarPolaritiesParams]:
  def getTypeVar: TypeVar = var_

  def setTypeVar(var_ : TypeVar): GetVarPolaritiesParams = GetVarPolaritiesParams(var_, pol)

  def getPolarity = pol
  def setPolarity(pol: Polarity) = GetVarPolaritiesParams(var_, pol)

/** Shadowing mode of the "get type variable polarities" operation. */
private object GetVarPolarities1 extends TypeShadowApplicator[Const[Polarities], GetVarPolaritiesParams](GetVarPolarities2):
  override def univ(univ: TUniv): Const[Polarities][Type] =
    Polarities.empty

/** Polarity mode of the "get type variable polarities" operation. */
private object GetVarPolarities2 extends TypePolarityApplicator[Const[Polarities], Const[Polarities], GetVarPolaritiesParams](GetVarPolarities3):
  override def bound(bound: Bound, params: GetVarPolaritiesParams): Polarities =
    val varPolarities = if bound.var_ == params.var_
      then Polarities.fromPolarity(params.pol)
      else Polarities.empty
    val pol = bound.dir match
      case Direction.Sub =>
        params.getPolarity.invert()
      case Direction.Super =>
        params.getPolarity
    val typePolarities = GetVarPolarities1.apply(bound.type_, params.setPolarity(pol))
    Polarities.join(varPolarities, typePolarities)

/** Dispatching mode of the "get type variable polarities" operation. */
private object GetVarPolarities3 extends TypeDispatcher[Const[Polarities], Const[Polarities], GetVarPolaritiesParams](TypeMonoidCombinator(JoinPolaritiesMonoid)):
  override def apply(type_ : Type, params: GetVarPolaritiesParams)(using first: TypeApplicator[Const[Polarities], GetVarPolaritiesParams] = this): Polarities =
    type_ match
      case TVar(var_) if var_ == params.var_ =>
        Polarities.fromPolarity(params.pol)
      case _ =>
        super.apply(type_, params)

  override def apply(bounds: List[Bound], params: GetVarPolaritiesParams): Polarities =
    JoinPolaritiesMonoid.combineMany(
      bounds.map(bound =>
        val varPolarities = if bound.var_ == params.var_
          then Polarities.fromPolarity(params.pol)
          else Polarities.empty
        val pol = bound.dir match
          case Direction.Sub =>
            params.getPolarity.invert()
          case Direction.Super =>
            params.getPolarity
        val typePolarities = GetVarPolarities1.apply(bound.type_, params.setPolarity(pol))
        Polarities.join(varPolarities, typePolarities)
      )
    )
