package hkmc2.ctml.core.type_

import hkmc2.ctml.core.*
import hkmc2.ctml.types.*

extension (clauses: Clauses)
  /** Check whether a type variable appears in the clauses. */
  def hasVar(var_ : TypeVar): Boolean =
    clauses.elems.exists(_.hasVar(var_))

extension (clause: Clause)
  /** Check whether a type variable appears in the clause. */
  def hasVar(var_ : TypeVar): Boolean =
    clause match
      case bound: Bound =>
        bound.hasVar(var_)
      case TermVarDecl(_, type_) =>
        type_.hasVar(var_)
      case _ =>
        false

extension (bound: Bound)
  /** Check whether a type variable appears in the bound. */
  def hasVar(var_ : TypeVar): Boolean =
    if bound.var_ == var_ then
      true
    else
      bound.type_.hasVar(var_)

extension (type_ : Type)
  /** Check whether a type variable appears in the type. */
  def hasVar(var_ : TypeVar): Boolean =
    type_ match
      case TBot =>
        false
      case TTop =>
        false
      case TVar(typeVar) if typeVar == var_ =>
        true
      case _: TVar =>
        false
      case TLam(param, ret) =>
        param.hasVar(var_) || ret.hasVar(var_)
      case TUnion(left, right) =>
        left.hasVar(var_) || right.hasVar(var_)
      case TInter(left, right) =>
        left.hasVar(var_) || right.hasVar(var_)
      case TConstrained(vars, base, bounds) =>
        if base.hasVar(var_) then
          false
        else
          bounds
            .map(_.hasVar(var_))
            .fold(false)(_ || _)
      case TConstraining(base, bounds) =>
        val baseHasVar    = base.hasVar(var_)
        val boundsHaveVar = bounds
          .map(_.hasVar(var_))
          .fold(false)(_ || _)
        baseHasVar || boundsHaveVar
