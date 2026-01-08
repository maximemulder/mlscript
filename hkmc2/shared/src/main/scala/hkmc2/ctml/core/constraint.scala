package hkmc2.ctml.core

import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*
import scala.collection.mutable.ListBuffer
import scala.util.chaining._

/** Make a constraining type, simplifying it if possible. */
def makeConstrainingType(type_ : Type, bounds: List[Bound]): Type =
  type_ match
    case TUniv(var_, body) =>
      TUniv(
        var_,
        makeConstrainingType(body, bounds)
      )
    case _ =>
      bounds match
        case Nil =>
          type_
        case bound :: bounds =>
          TConstraining(
            makeConstrainingType(type_, bounds),
            bound.toConstraint
          )

/** Make a constrained type, simplifying it if possible. */
def makeConstrainedType(type_ : Type, bounds: List[Bound]): Type =
  type_ match
    case TUniv(var_, body) =>
      TUniv(
        var_,
        makeConstrainedType(body, bounds)
      )
    case _ =>
      bounds match
        case Nil =>
          type_
        case bound :: bounds =>
          TConstrained(
            makeConstrainedType(type_, bounds),
            bound.toConstraint
          )

/** Make a constrained type by adding a lower bound to a type. */
def makeLowerBound(body: Type, var_ : TypeVar, type_ : Type): Type =
  val bound = Bound(var_, Direction.Super, type_)
  makeConstrainedType(body, List(bound))

/** Make a constrained type by adding an upper bound to a type. */
def makeUpperBound(body: Type, var_ : TypeVar, type_ : Type): Type =
  val bound = Bound(var_, Direction.Sub, type_)
  makeConstrainedType(body, List(bound))

/** Make a lambda type from its components, simplifying it if possible. */
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

extension (ctx: Context)
  /** Filter a list of subtyping constraints by removing the bounds that are already satisfied in
   *  the context. */
  def removeSatisfiedConstraints(constraints: List[Constraint]): List[Constraint] =
    constraints.filter(checkConstraint(_)(using ctx))
