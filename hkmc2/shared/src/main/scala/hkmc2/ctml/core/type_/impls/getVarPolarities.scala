package hkmc2.ctml.core.type_.impls

import hkmc2.ctml.core.debug.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

extension (type_ : Type)
  /** Get the polarities at which a type variable occurs in the type. */
  def getVarPolarities(var_ : TypeVar): Polarities =
    TypeVarPolarities(type_, TypeVarPolaritiesParams(var_, Polarity.Positive))

/** Parameters of the get type variable polarities operation. */
class TypeVarPolaritiesParams(val var_ : TypeVar, val pol: Polarity) extends WithPolarity[TypeVarPolaritiesParams]:
  def getPolarity = this.pol
  def setPolarity(pol: Polarity) = TypeVarPolaritiesParams(var_, pol)

/** Implementation of the get type variable polarities operation. */
object TypeVarPolarities extends TypePolarityDispatcher[Const[Polarities], Const[Polarities], TypeVarPolaritiesParams](TypeMonoidCombinator(JoinPolaritiesMonoid)):
  override def apply(type_ : Type, params: TypeVarPolaritiesParams)(using first: TypeApplicator[Const[Polarities], TypeVarPolaritiesParams] = this): Polarities =
    type_ match
      case TVar(var_) if var_ == params.var_ =>
        Polarities.fromPolarity(params.pol)
      case _ =>
        super.apply(type_, params)

  override def bound(bound: Bound, params: TypeVarPolaritiesParams): Polarities =
    val varPolarities = if bound.var_ == params.var_
      then Polarities.fromPolarity(params.pol)
      else Polarities.empty
    val typePolarities = this.apply(bound.type_, params)
    Polarities.join(varPolarities, typePolarities)
