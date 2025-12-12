package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.combine.getExtremalType

extension (type_ : Type)
  def removeVarDirectCycles(var_ : TypeVar, pol: Polarity): Type =
    type_.removeVarDirectCyclesImpl(var_, pol) match
      case Some(type_) =>
        type_
      case None =>
        getExtremalType(pol.dir)

  private def removeVarDirectCyclesImpl(var_ : TypeVar, pol: Polarity): Option[Type] =
    (type_, pol) match
      case (TVar(typeVar), _) if typeVar == var_ =>
        return None
      case (TUnion(left, right), Polarity.Positive) =>
        (left.removeVarDirectCyclesImpl(var_, pol), right.removeVarDirectCyclesImpl(var_, pol)) match
          case (Some(left), Some(right)) =>
            Some(TUnion(left, right))
          case (Some(left), None) =>
            Some(left)
          case (None, Some(right)) =>
            Some(right)
          case (None, None) =>
            None
      case (TInter(left, right), Polarity.Negative) =>
        (left.removeVarDirectCyclesImpl(var_, pol), right.removeVarDirectCyclesImpl(var_, pol)) match
          case (Some(left), Some(right)) =>
            Some(TInter(left, right))
          case (Some(left), None) =>
            Some(left)
          case (None, Some(right)) =>
            Some(right)
          case (None, None) =>
            None
      case _ =>
        Some(type_)
