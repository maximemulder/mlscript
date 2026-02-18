package hkmc2.ctml.core

import scala.collection.mutable.ListBuffer
import scala.util.chaining._

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.types.*

/** Make a negation type, simplifying it if possible. */
def makeNegationType(body: Type): Type =
  body.negate()

/** Make a constraining type, simplifying it if possible. */
def makeConstrainingType(type_ : Type, constraints: List[Constraint]): Type =
  type_ match
    case TUniv(var_, body) =>
      TUniv(
        var_,
        makeConstrainingType(body, constraints)
      )
    case _ =>
      constraints match
        case Nil =>
          type_
        case constraint :: constraints =>
          TConstraining(
            makeConstrainingType(type_, constraints),
            constraint
          )

/** Make a constrained type, simplifying it if possible. */
def makeConstrainedType(type_ : Type, constraints: List[Constraint]): Type =
  type_ match
    case TUniv(var_, body) =>
      TUniv(
        var_,
        makeConstrainedType(body, constraints)
      )
    case _ =>
      constraints match
        case Nil =>
          type_
        case constraint :: constraints =>
          TConstrained(
            makeConstrainedType(type_, constraints),
            constraint
          )

/** Make a lambda type, simplifying it if possible. */
def makeLambdaType(param: Type, ret: Type): Type =
  ret match
    case TUniv(var_, body) =>
      val type_ = makeLambdaType(param, body)
      TUniv(var_, type_)
    case _ =>
      TLam(param, ret)

/** Check whether or not a bound is implicit, that is, satisfied no matter the context. */
def isBoundImplicit(bound: Bound): Boolean =
  (bound.dir, bound.type_) match
    case (Direction.Sub, TTop) | (Direction.Super, TBot) =>
      true
    case (_, TVar(var_)) if var_ == bound.var_ =>
      true
    case _ =>
      false

/** Filter a list of bounds by removing the implicit bounds, that is, the bounds that are always
 *  satisfied no matter the context. */
def removeImplicitBounds(bounds: List[Bound]): List[Bound] =
  bounds.filter(!isBoundImplicit(_))

extension (ctx: Context)
  /** Filter a list of bounds by removing the bounds that are already satisfied in the context. */
  def removeSatisfiedBounds(bounds: List[Bound]): List[Bound] =
    bounds.filter(!ctx.checkBoundSatisfied(_))
