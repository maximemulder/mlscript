package hkmc2.ctml.core.type_

import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Replace a type variable by an other type variable in the type, without simplifying the resulting type. */
  def substitute(var_ : TypeVar, substitute: TypeVar): Type =
    type_ match
      case TVar(typeVar) =>
        TVar(typeVar.substitute(var_, substitute))
      case TConstrained(vars, base, bounds) =>
        val newBase   = base.substitute(var_, substitute)
        val newBounds = bounds.map(_.substitute(var_, substitute))
        TConstrained(vars, newBase, newBounds)
      case TConstraining(base, bounds) =>
        val newBase   = base.substitute(var_, substitute)
        val newBounds = bounds.map(_.substitute(var_, substitute))
        TConstraining(newBase, newBounds)
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

extension (constrained: TConstrained)
  /** Substitute the quantified variables in a constrained type with new fresh type variables. */
  def freshen(): TConstrained =
    constrained.vars.foldRight(constrained)((var_, constrained) =>
      val freshDecl = declNewFreshVar()
      val vars   = constrained.vars.map(_.substitute(var_, freshDecl.var_))
      val bounds = constrained.bounds.map(_.substitute(var_, freshDecl.var_))
      val base   = constrained.base.substitute(var_, freshDecl.var_)
      TConstrained(vars, base, bounds)
    )
