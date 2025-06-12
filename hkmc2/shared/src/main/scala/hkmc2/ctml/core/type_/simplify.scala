package hkmc2.ctml.core.type_

import hkmc2.ctml.core.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Simplify the type based on the information available in a context. */
  def simplify()(using ctx: Clauses): Type =
    type_ match
      case TLam(param, ret) =>
        val newParam = param.simplify()
        val newRet   = ret.simplify()
        TLam(newParam, newRet)
      case TUnion(left, right) =>
        val newLeft  = left.simplify()
        val newRight = right.simplify()
        join(left, right)
      case TInter(left, right) =>
        val newLeft  = left.simplify()
        val newRight = right.simplify()
        meet(newLeft, newRight)
      case TConstrained(vars, base, bounds) =>
        val newBase = base.simplify()
        TConstrained(vars, newBase, bounds)
      case TConstraining(base, bounds) =>
        val newBase = base.simplify()
        attachConstrainingBounds(newBase, bounds)
      case _ =>
        type_
