package hkmc2.ctml.core.var_

import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.config.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.SubtypingCache

extension (type_ : Type)
  def removeDirectVar(var_ : TypeVar, pol: Polarity): Type =
    type_.removeDirectVarImpl(var_, pol) match
      case Some(type_) =>
        type_
      case None =>
        getExtremalType(pol.dir)

  private def removeDirectVarImpl(var_ : TypeVar, pol: Polarity): Option[Type] =
    (type_, pol) match
      case (TVar(typeVar), _) if typeVar == var_ =>
        return None
      case (TUnion(left, right), Polarity.Positive) =>
        (left.removeDirectVarImpl(var_, pol), right.removeDirectVarImpl(var_, pol)) match
          case (Some(left), Some(right)) =>
            Some(TUnion(left, right))
          case (Some(left), None) =>
            Some(left)
          case (None, Some(right)) =>
            Some(right)
          case (None, None) =>
            None
      case (TInter(left, right), Polarity.Negative) =>
        (left.removeDirectVarImpl(var_, pol), right.removeDirectVarImpl(var_, pol)) match
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
