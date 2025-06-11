package hkmc2.ctml.core.type_

import hkmc2.ctml.core.*
import hkmc2.ctml.core.merge.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Replace a type variable by a subtitute type in a type. */
  def substitute(varName: String, substitute: Type)(using ctx: Clauses): Type =
    type_ match
      case TVar(typeVarName) if typeVarName == varName =>
        substitute
      case TLam(param, ret) =>
        val newParam = param.substitute(varName, substitute)
        val newRet   = ret.substitute(varName, substitute)
        TLam(newParam, newRet)
      case TUnion(left, right) =>
        val newLeft  = left.substitute(varName, substitute)
        val newRight = right.substitute(varName, substitute)
        join(newLeft, newRight)
      case TInter(left, right) =>
        val newLeft  = left.substitute(varName, substitute)
        val newRight = right.substitute(varName, substitute)
        join(newLeft, newRight)
      case TConstrained(vars, base, bounds) =>
        val newBase = base.substitute(varName, substitute)
        val newBounds = bounds.substitute(varName, substitute)
        TConstrained(vars, newBase, newBounds)
      case TConstraining(base, bounds) =>
        val newBase = base.substitute(varName, substitute)
        val newBounds = bounds.substitute(varName, substitute)
        attachConstrainingBounds(newBase, newBounds)
      case _ =>
        type_

extension (bounds: List[Bound])
  /** Substitute a variable by a type in the list of bounds. */
  def substitute(varName: String, substitute: Type)(using ctx: Clauses): List[Bound] =
    bounds.iterator
      .filter(_.name != varName)
      .map(_.substitute(varName, substitute))
      .toList

extension (bound: Bound)
  /** Substitute a variable by a type in the bound. */
  def substitute(varName: String, substitute: Type)(using ctx: Clauses): Bound =
    val newType = bound.type_.substitute(varName, substitute)
    Bound(bound.name, bound.dir, newType)
