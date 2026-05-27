package hkmc2.ctml.core.structural

import hkmc2.ctml.types.*

extension (type_ : Type)
  def removeDirectVar(var_ : TypeVar, pol: Polarity): Type =
    type_ match
      case TVar(typeVar) if var_ == typeVar =>
        getExtremalType(pol.dir)
      case TUnion(left, right) if pol == Polarity.Positive =>
        structuralCombine(
          left.removeDirectVar(var_, pol),
          right.removeDirectVar(var_, pol),
          Polarity.Positive,
        )
      case TInter(left, right) if pol == Polarity.Negative =>
        structuralCombine(
          left.removeDirectVar(var_, pol),
          right.removeDirectVar(var_, pol),
          Polarity.Negative,
        )
      case _ =>
        type_
