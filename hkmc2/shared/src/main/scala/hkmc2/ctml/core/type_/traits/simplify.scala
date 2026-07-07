package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*

/** Combinator that combines the components of a type into that type while simplifying it if
 *  possible by using the information available in the typing context. */
final class TypeSimplifyCombinator[P <: ContextParams[P]] extends TypeCombinator[Const[Type], Const[Constraint], P], ConstraintCombinator[Const[Type], Const[Constraint], P]:
  def bot(params: P): Type =
    TBot

  def top(params: P): Type =
    TTop

  def neg(body: Type, params: P): Type =
    simplifyNegation(body)

  def var_(var_ : TypeVar): Type =
    TVar(var_)

  def class_(var_ : ClassVar): TClass =
    TClass(var_)

  def tuple(left: Type, right: Type, p: P): TTuple =
    TTuple(left, right)

  def lam(param: Type, ret: Type, params: P): Type =
    simplifyLambda(param, ret)

  def joint(mode: JointMode, left: Type, right: Type, params: P): Type =
    simplifyJoint(mode, left, right)(using params.ctx)

  def app(abs: Type, arg: Type, params: P): Type =
    TApp(abs, arg)

  def univ(var_ : TypeVar, body: Type, params: P): Type =
    simplifyUniv(var_, body)(using params.ctx)

  def constrained(body: Type, constraint: Constraint, params: P): Type =
    simplifyConstrained(body, constraint)(using params.ctx)

  def constraining(body: Type, constraint: Constraint, params: P): Type =
    simplifyConstraining(body, constraint)(using params.ctx)

  def constraint(left: Type, dir: Direction, right: Type, p: P): Constraint =
    Constraint(left, dir, right)
