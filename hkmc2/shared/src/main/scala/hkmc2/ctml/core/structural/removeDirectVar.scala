package hkmc2.ctml.core.structural

import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Remove the direct occurences of some type variables in the type, that is, occurences not
   *  nested in a type constructor. */
  def removeDirectVars(vars: Iterable[TypeVar], pol: Polarity): Type =
    vars.foldLeft(type_)((bound, var_) => bound.removeDirectVar(var_, pol))

  /** Remove the direct occurences of a type variable in the type, that is, occurences not nested
   *  in a type constructor. */
  def removeDirectVar(var_ : TypeVar, pol: Polarity): Type =
    type_ match
      case TVar(typeVar) if var_ == typeVar =>
        getExtremalType(pol.dir)
      case TUnion(left, right) if pol == Polarity.Positive =>
        structuralCombine(
          JointMode.Union,
          left.removeDirectVar(var_, pol),
          right.removeDirectVar(var_, pol),
        )
      case TInter(left, right) if pol == Polarity.Negative =>
        structuralCombine(
          JointMode.Inter,
          left.removeDirectVar(var_, pol),
          right.removeDirectVar(var_, pol),
        )
      case _ =>
        type_
