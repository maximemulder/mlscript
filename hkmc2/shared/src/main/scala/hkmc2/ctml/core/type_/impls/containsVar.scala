package hkmc2.ctml.core.type_.impls

import hkmc2.ctml.config.*
import hkmc2.ctml.core.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*
import hkmc2.ctml.util.given

extension (type_ : Type)
  /** Check whether a type variable appears in the type. */
  def containsVar(var_ : TypeVar): Boolean =
    ContainsVar1(type_, ContainsVarParams(var_))

extension (clauses: Clauses)
  /** Check whether a type variable appears in the clauses. */
  def containsVar(var_ : TypeVar): Boolean =
    clauses.elems.exists(_.containsVar(var_))

extension (clause: Clause)
  /** Check whether a type variable appears in the clause. */
  def containsVar(var_ : TypeVar): Boolean =
    clause match
      case bound: Bound =>
        bound.containsVar(var_)
      case TermVarDecl(_, type_) =>
        type_.containsVar(var_)
      case _ =>
        false

extension (bound: Bound)
  /** Check whether a type variable appears in the bound. */
  def containsVar(var_ : TypeVar): Boolean =
    if bound.var_ == var_ then
      true
    else
      bound.type_.containsVar(var_)

extension (constraint: Constraint)
  /** Check whether a type variable appears in the constraint. */
  def containsVar(var_ : TypeVar): Boolean =
    constraint.left.containsVar(var_) || constraint.right.containsVar(var_)

/** Parameters of the "contains type variable" operation. */
private class ContainsVarParams(val var_ : TypeVar) extends TypeVarParams[ContainsVarParams]:
  override def setVar(var_ : TypeVar) = ContainsVarParams(var_)

/** Shadowing node of the "contains type variable" operation. */
private object ContainsVar1 extends TypeShadowApplicator[Const[Boolean], Const[Boolean], ContainsVarParams](ContainsVar2):
  def univ(univ: TUniv): Boolean =
    false

/** Variable checking node of the "contains type variable" operation. */
private object ContainsVar2 extends TypeChainApplicator[Const[Boolean], Const[Boolean], ContainsVarParams](ContainsVar3):
  override def apply(type_ : Type, p: ContainsVarParams)(using first: TypeApplicator[Const[Boolean], Const[Boolean], ContainsVarParams]): Boolean =
    type_ match
      case TVar(typeVar) if typeVar == p.var_ =>
        true
      case TUniv(typeVar, _) if typeVar == p.var_ =>
        false
      case TConstrained(body, constraint) =>
        first.apply(body, p) || constraint.containsVar(p.var_)
      case TConstraining(body, constraint) =>
        first.apply(body, p) || constraint.containsVar(p.var_)
      case _ =>
        next.apply(type_, p)

  override def apply(constraint: Constraint, p: ContainsVarParams)(using first: TypeApplicator[Const[Boolean], Const[Boolean], ContainsVarParams]): Boolean =
    false

/** Dispatching node of the "contains type variable" operation. */
private def ContainsVar3 = TypeDispatcher[Const[Boolean], Const[Boolean], ContainsVarParams](ContainsVar4)

/** Monoidal combination node of the "contains type variable" operation. */
private def ContainsVar4 = TypeMonoidCombinator[Boolean, ContainsVarParams](AnyMonoid)
