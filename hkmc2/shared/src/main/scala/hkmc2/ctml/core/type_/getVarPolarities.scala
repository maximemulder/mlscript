package hkmc2.ctml.core.type_

import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

extension (type_ : Type)
  /** Get the polarities at which a variable occurs in the type. */
  def getVarPolarities(var_ : TypeVar)(using polarity: Polarity): Polarities =
    given Monoid[Polarities] = JoinPolaritiesMonoid
    type_ match
      case TVar(typeVar) if typeVar == var_ =>
        Polarities.fromPolarity(polarity)
      case TLam(param, ret) =>
        val paramPolarities = param.getVarPolarities(var_)(using polarity.invert())
        val retPolarities   = ret.getVarPolarities(var_)
        Polarities.join(paramPolarities, retPolarities)
      case TConstrained(body, bounds) =>
        val bodyPolarities   = body.getVarPolarities(var_)
        val boundsPolarities = bounds.getVarPolarities(var_)
        Polarities.join(bodyPolarities, boundsPolarities)
      case TConstraining(body, bounds) =>
        val bodyPolarities   = body.getVarPolarities(var_)
        val boundsPolarities = bounds.getVarPolarities(var_)
        Polarities.join(bodyPolarities, boundsPolarities)
      case _ =>
        type_.accumulate(_.getVarPolarities(var_))

extension (bounds: List[Bound])
  /** Get the polarities at which a variable occurs in the list of bounds. */
  def getVarPolarities(var_ : TypeVar)(using polarity: Polarity): Polarities =
    bounds
      .map(_.getVarPolarities(var_))
      .fold(Polarities.empty)(Polarities.join)

extension (bound: Bound)
  /** Get the polarities at which a variable occurs in the bound. */
  def getVarPolarities(var_ : TypeVar)(using polarity: Polarity): Polarities =
    val boundPolarity = bound.dir match
      case Direction.Sub =>
        polarity.invert()
      case Direction.Super =>
        polarity

    val varPolarities = if bound.var_ == var_
      then Polarities.fromPolarity(boundPolarity)
      else Polarities.empty
    val typePolarities = bound.type_.getVarPolarities(var_)(using boundPolarity)
    Polarities.join(varPolarities, typePolarities)
