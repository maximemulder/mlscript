package hkmc2.ctml.core.type_

import hkmc2.ctml.core.*
import hkmc2.ctml.core.debug.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*
import hkmc2.ctml.util.given

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
    given Monoid[Boolean] = AnyMonoid
    type_ match
      case TVar(typeVar) if typeVar == var_ =>
        true
      case TUniv(typeVar, _) if typeVar == var_ =>
        false
      case TConstrained(base, bounds) =>
        base.hasVar(var_) || bounds.map(bound => if bound.var_ == var_ then false else bound.type_.hasVar(var_)).foldM()
      case TConstraining(base, bounds) =>
        base.hasVar(var_) || bounds.map(_.hasVar(var_)).foldM()
      case _ =>
        type_.accumulate(_.hasVar(var_))
