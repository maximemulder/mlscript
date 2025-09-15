package hkmc2.ctml.core.type_

import hkmc2.ctml.core.*
import hkmc2.ctml.core.debug.*
import hkmc2.ctml.core.type_.traits.*
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
    N1(type_, Params(var_))

/** Parameters of the has variable operation. */
private class Params(val var_ : TypeVar) extends WithTypeVar[Params]:
  def getTypeVar = var_
  def setTypeVar(var_ : TypeVar) = Params(var_)

/** Shadowing node of the has variable operation. */
private object N1 extends TypeShadowApplicator[Const[Boolean], Params](N2):
  def univ(univ: TUniv): Boolean =
    false

/** Variable checking node of the has variable operation. */
private object N2 extends TypeChainApplicator[Const[Boolean], Params](N3):
  def apply(type_ : Type, p: Params)(using first: TypeApplicator[Const[Boolean], Params]): Boolean =
    type_ match
      case TVar(typeVar) if typeVar == p.var_ =>
        true
      case TUniv(typeVar, _) if typeVar == p.var_ =>
        false
      case TConstrained(body, bounds) =>
        first.apply(body, p) || bounds.map(bound => if bound.var_ == p.var_ then false else first.apply(bound.type_, p)).foldM()(using AnyMonoid)
      case TConstraining(body, bounds) =>
        first.apply(body, p) || bounds.map(_.hasVar(p.var_)).foldM()(using AnyMonoid)
      case _ =>
        next.apply(type_, p)

/** Dispatching node of the has variable operation. */
private object N3 extends TypeDispatcher[Const[Boolean], Const[Boolean], Params](N4):
      def apply(bounds: List[Bound], p: Params): Boolean =
        false

/** Monoidal combination node of the has variable operation. */
private object N4 extends TypeMonoidCombinator[Boolean, Params](AnyMonoid)
