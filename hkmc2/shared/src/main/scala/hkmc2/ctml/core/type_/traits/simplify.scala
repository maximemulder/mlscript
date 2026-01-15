package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.core.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*
import hkmc2.ctml.core.config.debug

/** Combinator that combines the components of a type into that type while simplifying it if
 *  possible by using the information available in the typing context. */
final class TypeSimplifyCombinator[P <: ContextParams[P]] extends TypeCombinator[Const[Type], Const[Constraint], P], ConstraintCombinator[Const[Type], Const[Constraint], P]:
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

  def constrained(body: Type, constraint: Constraint, params: P): Type =
    val filteredConstraints = params.ctx.removeSatisfiedConstraints(List(constraint))
    makeConstrainedType(body, filteredConstraints)

  def constraining(body: Type, constraint: Constraint, params: P): Type =
    val filteredConstraints = params.ctx.removeSatisfiedConstraints(List(constraint))
    makeConstrainingType(body, filteredConstraints)

  def constraint(left: Type, dir: Direction, right: Type, p: P): Constraint =
    Constraint(left, dir, right)
