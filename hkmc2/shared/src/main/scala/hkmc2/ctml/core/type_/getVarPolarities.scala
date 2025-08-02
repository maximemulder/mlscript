package hkmc2.ctml.core.type_

import hkmc2.ctml.core.debug.*
import hkmc2.ctml.core.abstractions.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

extension (type_ : Type)
  /** Get the polarities at which a type variable occurs in the type. */
  def getVarPolarities(var_ : TypeVar): Polarities =
    VarPolarities.apply(type_, VarPolaritiesParams(var_, Polarity.Positive))

class VarPolaritiesParams(val var_ : TypeVar, val pol: Polarity) extends WithPolarity[VarPolaritiesParams]:
  def getPolarity = this.pol
  def setPolarity(pol: Polarity) = VarPolaritiesParams(var_, pol)

object VarPolarities extends TypePolarityDispatcher[Const[Polarities], Const[Polarities], VarPolaritiesParams](TypeMonoidCombinator(JoinPolaritiesMonoid)):
  override def apply(type_ : Type, params: VarPolaritiesParams): Polarities =
    type_ match
      case TVar(var_) if var_ == params.var_ =>
        Polarities.fromPolarity(params.pol)
      case _ =>
        super.apply(type_, params)

  override def bound(bound: Bound, params: VarPolaritiesParams): Polarities =
    val varPolarities = if bound.var_ == params.var_
      then Polarities.fromPolarity(params.pol)
      else Polarities.empty
    val typePolarities = this.apply(bound.type_, params)
    Polarities.join(varPolarities, typePolarities)
