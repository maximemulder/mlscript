package hkmc2.ctml.core.var_

import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.config.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.VarCache

extension (type_ : Type)
  /** Remove cyclic type variable constraints in a type, that is, when a type variable is
   *  constrained to be a subtype of supertype of itself.
   *
   *  The subtyping direction refers to the type variable on the left and the type on the right.
   */
  def removeCyclicVar(var_ : TypeVar, dir: Direction)(using Context, VarCache): Type =
    type_ match
      case TVar(typeVar) if typeVar == var_ =>
        getExtremalType(dir)
      case TUnion(left, right) if dir == Direction.Super =>
        join(
          left.removeCyclicVar(var_, dir),
          right.removeCyclicVar(var_, dir)
        )
      case TInter(left, right) if dir == Direction.Sub =>
        meet(
          left.removeCyclicVar(var_, dir),
          right.removeCyclicVar(var_, dir)
        )
      case _ =>
        type_
