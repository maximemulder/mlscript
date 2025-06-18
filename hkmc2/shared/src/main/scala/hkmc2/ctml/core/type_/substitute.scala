package hkmc2.ctml.core.type_

import hkmc2.ctml.core.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Replace a type variable by a subtitute type in a type. */
  def substitute(var_ : TVar, substitute: Type)(using ctx: Context): Type =
    type_ match
      case typeVar : TVar if typeVar == var_ =>
        substitute
      case TLam(param, ret) =>
        val newParam = param.substitute(var_, substitute)
        val newRet   = ret.substitute(var_, substitute)
        TLam(newParam, newRet)
      case TUnion(left, right) =>
        val newLeft  = left.substitute(var_, substitute)
        val newRight = right.substitute(var_, substitute)
        join(newLeft, newRight)
      case TInter(left, right) =>
        val newLeft  = left.substitute(var_, substitute)
        val newRight = right.substitute(var_, substitute)
        join(newLeft, newRight)
      case TConstrained(vars, base, bounds) =>
        val newBase = base.substitute(var_, substitute)
        val newBounds = bounds.substitute(var_, substitute)
        TConstrained(vars, newBase, newBounds)
      case TConstraining(base, bounds) =>
        val newBase = base.substitute(var_, substitute)
        val newBounds = bounds.substitute(var_, substitute)
        attachConstrainingBounds(newBase, newBounds)
      case _ =>
        type_

extension (bounds: List[Bound])
  /** Substitute a type variable by a type in the list of bounds. */
  def substitute(var_ : TVar, substitute: Type)(using ctx: Context): List[Bound] =
    bounds.iterator
      .filter(_.var_ != var_)
      .map(_.substitute(var_, substitute))
      .toList

extension (bound: Bound)
  /** Substitute a type variable by a type in the bound. */
  def substitute(var_ : TVar, substitute: Type)(using ctx: Context): Bound =
    val newType = bound.type_.substitute(var_, substitute)
    Bound(bound.var_, bound.dir, newType)
