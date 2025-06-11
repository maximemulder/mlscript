package hkmc2.ctml.core.type_

import hkmc2.ctml.types.*

/** Get the polarities at which a variable occurs in a type. */
def getTypePolarities(type_ : Type, varName : String)(using polarity: Polarity): Polarities =
  type_ match
    case _ : TBot | TTop =>
      Polarities.empty
    case var_ : TVar if var_.name == varName =>
      Polarities.fromPolarity(polarity)
    case _ : TVar =>
      Polarities.empty
    case lam : TLam =>
      val paramOccurences =
        given Polarity = polarity.invert()
        getTypePolarities(lam.param, varName)
      val retOccurences = getTypePolarities(lam.ret, varName)
      Polarities.join(paramOccurences, retOccurences)
    case union : TUnion =>
      val leftOccurences  = getTypePolarities(union.left, varName)
      val rightOccurences = getTypePolarities(union.right, varName)
      Polarities.join(leftOccurences, rightOccurences)
    case inter : TInter =>
      val leftOccurences  = getTypePolarities(inter.left, varName)
      val rightOccurences = getTypePolarities(inter.right, varName)
      Polarities.join(leftOccurences, rightOccurences)
    case constrained: TConstrained =>
      val baseOccurences   = getTypePolarities(constrained.base, varName)
      val boundsOccurences = constrained.bounds
        .map(getBoundPolarities(_, varName))
        .fold(Polarities.empty)(Polarities.join)
      Polarities.join(baseOccurences, boundsOccurences)
    case constraining: TConstraining =>
      val baseOccurences   = getTypePolarities(constraining.base, varName)
      val boundsOccurences = constraining.bounds
        .map(getBoundPolarities(_, varName))
        .fold(Polarities.empty)(Polarities.join)
      Polarities.join(baseOccurences, boundsOccurences)

/** Get the polarities at which a variable occurs in a bound. */
def getBoundPolarities(bound: Bound, varName : String)(using polarity: Polarity): Polarities =
  val boundPolarity = bound.dir match
    case Direction.Sub =>
      polarity.invert()
    case Direction.Super =>
      polarity

  val varOccurences = if bound.name == varName
    then Polarities.fromPolarity(boundPolarity)
    else Polarities.empty
  given Polarity = boundPolarity
  val typeOccurences = getTypePolarities(bound.type_, varName)
  Polarities.join(varOccurences, typeOccurences)
