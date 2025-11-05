package hkmc2.ctml.core.type_.impls

import hkmc2.ctml.util.OrderedSet as MutSet

import hkmc2.ctml.core.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.debug.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*
import hkmc2.ctml.util.given

extension (type_ : Type)
  /** Check whether a type variable is used in the type. */
  def usesVar(var_ : TypeVar)(using ctx: Context): Boolean =
    type_.usesVar(UsesVarParams(var_, ctx, MutSet()))

  /** Check whether a type variable is used in the type. */
  private def usesVar(params: UsesVarParams): Boolean =
    UsesVar1(type_, params)

extension (clauses: Clauses)
  /** Check whether a type variable is used in the clauses. */
  private def usesVar(params: UsesVarParams): Boolean =
    clauses.elems.exists(_.usesVar(params))

extension (clause: Clause)
  /** Check whether a type variable is used in the clause. */
  private def usesVar(params: UsesVarParams): Boolean =
    clause match
      case bound: Bound =>
        bound.usesVar(params)
      case TermVarDecl(_, type_) =>
        type_.usesVar(params)
      case _ =>
        false

extension (bound: Bound)
  /** Check whether a type variable is used in the bound. */
  private def usesVar(params: UsesVarParams): Boolean =
    if bound.var_ == params.var_ then
      true
    else
      bound.type_.usesVar(params: UsesVarParams)

/** Parameters of the "uses type variable" operation. */
private class UsesVarParams(val var_ : TypeVar, val ctx: Context, val cache: MutSet[TypeVar]) extends WithTypeVar[UsesVarParams]:
  def getTypeVar = var_
  def setTypeVar(var_ : TypeVar) = UsesVarParams(var_, ctx, cache)

/** Shadowing node of the "uses type variable" operation. */
private object UsesVar1 extends TypeShadowApplicator[Const[Boolean], UsesVarParams](UsesVar2):
  def univ(univ: TUniv): Boolean =
    false

/** Variable checking node of the "uses type variable" operation. */
private object UsesVar2 extends TypeChainApplicator[Const[Boolean], UsesVarParams](UsesVar3):
  def apply(type_ : Type, params: UsesVarParams)(using first: TypeApplicator[Const[Boolean], UsesVarParams]): Boolean =
    type_ match
      case TVar(typeVar) if typeVar == params.var_ =>
        true
      case TVar(typeVar) =>
        if params.cache.contains(typeVar) then
          false
        else
          params.cache.add(typeVar)
          val bounds = params.ctx.varBounds(typeVar)
          bounds.map(_.usesVar(params)).foldM()(using AnyMonoid)
      case TUniv(typeVar, _) if typeVar == params.var_ =>
        false
      case TConstrained(body, bound) =>
        first.apply(body, params) || (if bound.var_ == params.var_ then false else first.apply(bound.type_, params))
      case TConstraining(body, bound) =>
        first.apply(body, params) || bound.containsVar(params.var_)
      case _ =>
        next.apply(type_, params)

/** Dispatching node of the "uses type variable" operation. */
private object UsesVar3 extends TypeDispatcher[Const[Boolean], Const[Boolean], UsesVarParams](UsesVar4):
  override def apply(bound: Bound, params: UsesVarParams): Boolean =
    false

/** Monoidal combination node of the "uses type variable" operation. */
private object UsesVar4 extends TypeMonoidCombinator[Boolean, UsesVarParams](AnyMonoid)
