package hkmc2.ctml.core.type_

import hkmc2.ctml.core.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Replace a type variable by a substitute type in a type, simplifying the resulting type if possible. */
  def inline(var_ : TypeVar, substitute: Type)(using ctx: Context): Type =
    type_ match
      case TVar(typeVar) if typeVar == var_ =>
        substitute
      case TConstrained(body, bounds) =>
        val newBody   = body.inline(var_, substitute)
        val newBounds = bounds.inline(var_, substitute)
        TConstrained(newBody, newBounds)
      case TConstraining(body, bounds) =>
        val newBody   = body.inline(var_, substitute)
        val newBounds = bounds.inline(var_, substitute)
        attachConstrainingBounds(newBody, newBounds)
      case _ =>
        type_.mapSimplify(_.inline(var_, substitute))

extension (bounds: List[Bound])
  /** Substitute a type variable by a type in the list of bounds. */
  def inline(var_ : TypeVar, substitute: Type)(using ctx: Context): List[Bound] =
    bounds.iterator
      .filter(_.var_ != var_)
      .map(_.inline(var_, substitute))
      .toList

extension (bound: Bound)
  /** Substitute a type variable by a type in the bound. */
  def inline(var_ : TypeVar, substitute: Type)(using ctx: Context): Bound =
    val newType = bound.type_.inline(var_, substitute)
    Bound(bound.var_, bound.dir, newType)
