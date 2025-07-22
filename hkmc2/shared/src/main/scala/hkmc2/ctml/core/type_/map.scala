package hkmc2.ctml.core.type_

import hkmc2.ctml.core.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Map over the direct components of a type. */
  def map(f: Type => Type): Type =
    type_ match
      case TBot | TTop | TVar(_) =>
        type_
      case TLam(param, ret) =>
        TLam(f(param), f(ret))
      case TUnion(left, right) =>
        TUnion(f(left), f(right))
      case TInter(left, right) =>
        TInter(f(left), f(right))
      case TUniv(var_, body) =>
        TUniv(var_, f(body))
      case TConstrained(body, bounds) =>
        TConstrained(f(body), bounds.map(_.map(f)))
      case TConstraining(body, bounds) =>
        TConstraining(f(body), bounds.map(_.map(f)))

extension (bound: Bound)
  /** Map over the direct components of a bound. */
  def map(f: Type => Type): Bound =
    Bound(bound.var_, bound.dir, f(bound.type_))

extension(type_ : Type)(using ctx: Context)
  /** Map over the direct components of a type and simplify the result. */
  def mapSimplify(f: Type => Type): Type =
    type_ match
      case TBot | TTop | TVar(_) =>
        type_
      case TLam(param, ret) =>
        TLam(f(param), f(ret))
      case TUnion(left, right) =>
        join(f(left), f(right))
      case TInter(left, right) =>
        meet(f(left), f(right))
      case TUniv(var_, body) =>
        given Context = ctx.extend(declRigidVar(var_))
        TUniv(var_, f(body))
      case TConstrained(body, bounds) =>
        TConstrained(f(body), bounds.map(_.map(f)))
      case TConstraining(body, bounds) =>
        attachConstrainingBounds(f(body), bounds.map(_.map(f)))
