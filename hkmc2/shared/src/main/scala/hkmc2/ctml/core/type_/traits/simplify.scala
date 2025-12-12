package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.core.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

/** Combinator that combines the components of a type into that type while simplifying it if
 *  possible by using the information available in the typing context. */
final class TypeSimplifyCombinator[P <: ContextParams[P]] extends TypeCombinator[Const[Type], Id, P]:
  def bot(params: P): Type =
    TBot

  def top(params: P): Type =
    TTop

  def var_(var_ : TypeVar): Type =
    TVar(var_)

  def tuple(left: Type, right: Type, p: P): TTuple =
    TTuple(left, right)

  def lam(param: Type, ret: Type, params: P): Type =
    makeLambdaType(param, ret)

  def union(left: Type, right: Type, params: P): Type =
    join(left, right)(using params.ctx, VarCache())

  def inter(left: Type, right: Type, params: P): Type =
    meet(left, right)(using params.ctx, VarCache())

  def app(abs: Type, arg: Type, params: P): Type =
    TApp(abs, arg)

  def univ(var_ : TypeVar, body: Type, params: P): Type =
    TUniv(var_, body)

  def constrained(body: Type, bound: Bound, params: P): Type =
    val filteredBounds = params.ctx.removeSatisfiedBounds(List(bound))
    makeConstrainedType(body, filteredBounds)

  def constraining(body: Type, bound: Bound, params: P): Type =
    val filteredBounds = params.ctx.removeSatisfiedBounds(List(bound))
    makeConstrainingType(body, filteredBounds)
