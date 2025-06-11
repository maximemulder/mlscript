package hkmc2.ctml.core.type_

import hkmc2.ctml.core.*
import hkmc2.ctml.types.*

extension (clauses: Clauses)
  /** Check whether a type variable appears in the clauses. */
  def hasVar(varName: String): Boolean =
    clauses.elems.exists(_.hasVar(varName))

extension (clause: Clause)
  /** Check whether a type variable appears in the clause. */
  def hasVar(varName: String): Boolean =
    clause match
      case bound: Bound =>
        bound.hasVar(varName)
      case var_ : TermVar =>
        var_.type_.hasVar(varName)
      case _ =>
        false

extension (bound: Bound)
  /** Check whether a type variable appears in the bound. */
  def hasVar(varName: String): Boolean =
    if bound.name == varName then
      true
    else
      bound.type_.hasVar(varName)

extension (type_ : Type)
  /** Check whether a type variable appears in the type. */
  def hasVar(varName: String): Boolean =
    type_ match
      case TBot =>
        false
      case TTop =>
        false
      case TVar(name) if name == varName =>
        true
      case _: TVar =>
        false
      case lam: TLam =>
        val param = lam.param.hasVar(varName)
        val ret   = lam.ret.hasVar(varName)
        param || ret
      case union: TUnion =>
        val left  = union.left.hasVar(varName)
        val right = union.right.hasVar(varName)
        left || right
      case inter: TInter =>
        val left  = inter.left.hasVar(varName)
        val right = inter.right.hasVar(varName)
        left || right
      case constrained: TConstrained =>
        if constrained.base.hasVar(varName) then
          false
        else
          constrained.bounds
            .map(_.hasVar(varName))
            .fold(false)(_ || _)
      case constraining: TConstraining =>
        val base   = constraining.base.hasVar(varName)
        val bounds = constraining.bounds
          .map(_.hasVar(varName))
          .fold(false)(_ || _)
        base || bounds
