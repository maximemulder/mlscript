package hkmc2.ctml.core.type_

import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Get the polarities at which a variable occurs in the type. */
  def getVarPolarities(varName : String)(using polarity: Polarity): Polarities =
    type_ match
      case TVar(typeVarName) if typeVarName == varName =>
        Polarities.fromPolarity(polarity)
      case TLam(param, ret) =>
        val paramPolarities = param.getVarPolarities(varName)(using polarity.invert())
        val retPolarities   = ret.getVarPolarities(varName)
        Polarities.join(paramPolarities, retPolarities)
      case TUnion(left, right) =>
        val leftPolarities  = left.getVarPolarities(varName)
        val rightPolarities = right.getVarPolarities(varName)
        Polarities.join(leftPolarities, rightPolarities)
      case TInter(left, right) =>
        val leftPolarities  = left.getVarPolarities(varName)
        val rightPolarities = right.getVarPolarities(varName)
        Polarities.join(leftPolarities, rightPolarities)
      case TConstrained(vars, base, bounds) =>
        val basePolarities   = base.getVarPolarities(varName)
        val boundsPolarities = bounds.getVarPolarities(varName)
        Polarities.join(basePolarities, boundsPolarities)
      case TConstraining(base, bounds) =>
        val basePolarities   = base.getVarPolarities(varName)
        val boundsPolarities = bounds.getVarPolarities(varName)
        Polarities.join(basePolarities, boundsPolarities)
      case _ =>
        Polarities.empty

extension (bounds: List[Bound])
  /** Get the polarities at which a variable occurs in the list of bounds. */
  def getVarPolarities(varName : String)(using polarity: Polarity): Polarities =
    bounds
      .map(_.getVarPolarities(varName))
      .fold(Polarities.empty)(Polarities.join)

extension (bound: Bound)
  /** Get the polarities at which a variable occurs in the bound. */
  def getVarPolarities(varName : String)(using polarity: Polarity): Polarities =
    val boundPolarity = bound.dir match
      case Direction.Sub =>
        polarity.invert()
      case Direction.Super =>
        polarity

    val varPolarities = if bound.name == varName
      then Polarities.fromPolarity(boundPolarity)
      else Polarities.empty
    val typePolarities = bound.type_.getVarPolarities(varName)(using boundPolarity)
    Polarities.join(varPolarities, typePolarities)
