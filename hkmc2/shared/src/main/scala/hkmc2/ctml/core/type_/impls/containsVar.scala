package hkmc2.ctml.core.type_.impls

import hkmc2.ctml.core.*
import hkmc2.ctml.core.debug.*
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

/** Parameters of the "contains type variable" operation. */
private class ContainsVarParams(val var_ : TypeVar) extends WithTypeVar[ContainsVarParams]:
  def getTypeVar = var_
  def setTypeVar(var_ : TypeVar) = ContainsVarParams(var_)

/** Shadowing node of the "contains type variable" operation. */
private object ContainsVar1 extends TypeShadowApplicator[Const[Boolean], ContainsVarParams](ContainsVar2):
  def univ(univ: TUniv): Boolean =
    false

/** Variable checking node of the "contains type variable" operation. */
private object ContainsVar2 extends TypeChainApplicator[Const[Boolean], ContainsVarParams](ContainsVar3):
  def apply(type_ : Type, p: ContainsVarParams)(using first: TypeApplicator[Const[Boolean], ContainsVarParams]): Boolean =
    type_ match
      case TVar(typeVar) if typeVar == p.var_ =>
        true
      case TUniv(typeVar, _) if typeVar == p.var_ =>
        false
      case TConstrained(body, bound) =>
        first.apply(body, p) || (if bound.var_ == p.var_ then false else first.apply(bound.type_, p))
      case TConstraining(body, bounds) =>
        first.apply(body, p) || bounds.map(_.containsVar(p.var_)).foldM()(using AnyMonoid)
      case _ =>
        next.apply(type_, p)

/** Dispatching node of the "contains type variable" operation. */
private object ContainsVar3 extends TypeDispatcher[Const[Boolean], Const[Boolean], ContainsVarParams](ContainsVar4):
  def apply(bounds: Bound, p: ContainsVarParams): Boolean =
    false

/** Monoidal combination node of the "contains type variable" operation. */
private object ContainsVar4 extends TypeMonoidCombinator[Boolean, ContainsVarParams](AnyMonoid)
