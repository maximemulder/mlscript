package hkmc2.ctml.core

import hkmc2.ctml.types.*

/** Get the polarities at which a variable occurs in a type. */
def getVarPolarities(type_ : Type, varName : String)(using polarity: Polarity): Polarities =
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
        getVarPolarities(lam.param, varName)
      val retOccurences = getVarPolarities(lam.ret, varName)
      Polarities.join(paramOccurences, retOccurences)
    case union : TUnion =>
      val leftOccurences  = getVarPolarities(union.left, varName)
      val rightOccurences = getVarPolarities(union.right, varName)
      Polarities.join(leftOccurences, rightOccurences)
    case inter : TInter =>
      val leftOccurences  = getVarPolarities(inter.left, varName)
      val rightOccurences = getVarPolarities(inter.right, varName)
      Polarities.join(leftOccurences, rightOccurences)
    case constraining: TConstraining =>
      // TODO: Handle polarities in constraints.
      getVarPolarities(constraining.base, varName)
