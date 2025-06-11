package hkmc2.ctml.core.type_

import hkmc2.ctml.core.*
import hkmc2.ctml.core.merge.*
import hkmc2.ctml.types.*

/** Simplify a type based on the information available in a context. */
def simplify(type_ : Type)(using ctx: Clauses): Type =
  type_ match
    case TBot | TTop | _: TVar =>
      type_
    case lam: TLam =>
      val param = simplify(lam.param)
      val ret   = simplify(lam.ret)
      TLam(param, ret)
    case union: TUnion =>
      val left  = simplify(union.left)
      val right = simplify(union.right)
      join(left, right)
    case inter: TInter =>
      val left  = simplify(inter.left)
      val right = simplify(inter.right)
      meet(left, right)
    case constrained: TConstrained =>
      val base = simplify(constrained.base)
      TConstrained(constrained.vars, base, constrained.bounds)
    case constraining: TConstraining =>
      val base = simplify(constraining.base)
      attachConstrainingBounds(base, constraining.bounds)
