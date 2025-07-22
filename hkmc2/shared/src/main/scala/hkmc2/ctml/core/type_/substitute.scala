package hkmc2.ctml.core.type_

import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Replace a type variable by an other type variable in the type, without simplifying the resulting type. */
  def substitute(var_ : TypeVar, substitute: TypeVar): Type =
    type_ match
      case TVar(typeVar) =>
        TVar(typeVar.substitute(var_, substitute))
      case TUniv(typeVar, body) if var_ == typeVar =>
        TUniv(typeVar, body)
      case TConstrained(body, bounds) =>
        val newBody = body.substitute(var_, substitute)
        val newBounds = bounds.map(_.substitute(var_, substitute))
        TConstrained(newBody, newBounds)
      case TConstraining(body, bounds) =>
        val newBody = body.substitute(var_, substitute)
        val newBounds = bounds.map(_.substitute(var_, substitute))
        TConstraining(newBody, newBounds)
      case _ =>
        type_.map(_.substitute(var_, substitute))

extension (bound: Bound)
  /** Substitute a type variable by another type variable in the bound. */
  def substitute(var_ : TypeVar, substitute: TypeVar): Bound =
    val newVar  = bound.var_.substitute(var_, substitute)
    val newType = bound.type_.substitute(var_, substitute)
    Bound(newVar, bound.dir, newType)

extension (typeVar: TypeVar)
  /** Substitute the type variable by another type variable if these are equal. */
  def substitute(var_ : TypeVar, substitute: TypeVar): TypeVar =
    if typeVar == var_ then
      substitute
    else
      typeVar

extension (univ: TUniv)
  /** Substitute the quantified variable of a universal type with a new fresh type variable. */
  def freshen(): TUniv =
    val freshDecl = declNewFreshVar()
    val body = univ.body.substitute(univ.var_, freshDecl.var_)
    TUniv(freshDecl.var_, body)
