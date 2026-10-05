package hkmc2.ctml.core

import scala.collection.mutable.ListBuffer
import scala.util.chaining._

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.types.*

/** Make a negation type, simplifying it if possible. */
def makeNegationType(body: Type): Type =
  body.negate()

/** Make a constraining type, simplifying it if possible.
 *
 *  A constraining type is the dual of a constrained type: it pairs a type with constraints that
 *  must be established to introduce it, and that may be assumed to eliminate it. It is not a
 *  primitive type, but is encoded as a negated constrained type `¬({c} ⟹ ¬τ)`, from which its
 *  subtyping rules are derived (see the contraposition of negated binder types in `subtypeImpl`).
 *
 *  Nested constraining types are flattened by `makeConstrainedType`, since the negation of the
 *  inner constraining type `¬¬({c'} ⟹ ¬τ)` simplifies to the inner constrained type. */
def makeConstrainingType(type_ : Type, constraints: List[Constraint]): Type =
  type_ match
    case TUniv(var_, body) =>
      TUniv(
        var_,
        makeConstrainingType(body, constraints)
      )
    case _ =>
      makeNegationType(makeConstrainedType(makeNegationType(type_), constraints))

/** Make a constrained type, simplifying it if possible. */
def makeConstrainedType(type_ : Type, constraints: List[Constraint]): Type =
  type_ match
    case TUniv(var_, body) =>
      TUniv(
        var_,
        makeConstrainedType(body, constraints)
      )
    case _ =>
      val (body, nestedConstraints) = type_.getConstrainedComponents
      val constraints2 = (constraints ::: nestedConstraints).distinct
      constraints2 match
        case Nil =>
          body
        case constraint :: constraints =>
          TConstrained(
            makeConstrainedType(body, constraints),
            constraint
          )

/** Make a joint type, simplifying it syntactically, that is, without any subtyping check: the
 *  identity of the joint is dropped, its absorbing element absorbs the other operand, and the left
 *  operand is dropped if it is already a component of the right operand. Unlike `combine`, this
 *  does not detect subsumed operands, but is cheap. */
def makeJointType(mode: JointMode, left: Type, right: Type): Type =
  val (identity, absorbing) = mode match
    case JointMode.Union =>
      (TBot, TTop)
    case JointMode.Inter =>
      (TTop, TBot)
  if left == identity || right == absorbing || right.getJointComponents(mode).contains(left) then
    right
  else if right == identity || left == absorbing then
    left
  else
    TJointType(mode, left, right)

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

extension (ctx: SubContext)
  /** Filter a list of bounds by removing the bounds that are already satisfied in the context. */
  def removeSatisfiedBounds(bounds: List[Bound]): List[Bound] =
    bounds.filter(!checkBound(_)(using ctx))
