package hkmc2.ctml.core.var_

import scala.util.boundary
import scala.util.boundary.break

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*
import hkmc2.ctml.util.given

extension (ctx: Context)
  /** Compare the level of two type variables within a context. */
  def compareVarLevels(left: TypeVar, right: TypeVar): Order =
    // If both variables are the same, their level is equal.
    if left == right then
      return Order.Equal

    boundary:
      for decl <- ctx.typeVarDecls do
        // If left appears first, it has a higher level than right.
        if decl.var_ == left then
          break(Order.Greater)

        // If right appears first, it has a higher level than left.
        if decl.var_ == right then
          break(Order.Lesser)

      throw new TypeError(Some(s"Type variable '${left}' or '${right}' not found in the context."))

  /** Check whether a type variable is recursive, that is, whether it appears in its own bounds. */
  def isVarRecursive(var_ : TypeVar): Boolean =
    ctx.varBounds(var_)
      .map((bound) => bound.type_.hasVar(var_))
      .foldM()(using AnyMonoid)
