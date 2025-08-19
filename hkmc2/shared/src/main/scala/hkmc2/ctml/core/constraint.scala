package hkmc2.ctml.core

import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*
import scala.collection.mutable.ListBuffer

extension (type_ : Type)
  /** Split the body type and constrained bounds of the type. */
  def splitConstrained(): (Type, List[Bound]) =
    type_ match
      case TConstrained(body, bounds) =>
        (body, bounds)
      case _ =>
        (type_, Nil)

  /** Split the body type and constraining bounds of the type. */
  def splitConstrainings(): (Type, List[Bound]) =
    type_ match
      case TConstraining(body, bounds) =>
        (body, bounds)
      case _ =>
        (type_, Nil)

/** Make a constraining type, simplifying it if possible. */
def makeConstrainingType(type_ : Type, bounds: List[Bound]): Type =
  val filteredBounds = removeImplicitBounds(bounds)
  filteredBounds match
    case Nil =>
      type_
    case _ =>
      type_ match
        case TUniv(var_, body) =>
          val type_ = makeConstrainingType(body, filteredBounds)
          TUniv(var_, type_)
        case _ =>
          TConstraining(type_, filteredBounds)

/** Make a constrained type, simplifying it if possible. */
def makeConstrainedType(type_ : Type, bounds: List[Bound]): Type =
  val filteredBounds = removeImplicitBounds(bounds)
  filteredBounds match
    case Nil =>
      type_
    case _ =>
      type_ match
        case TUniv(var_, body) =>
          val type_ = makeConstrainedType(body, filteredBounds)
          TUniv(var_, type_)
        case _ =>
          TConstrained(type_, filteredBounds)

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
/** Remove the implicit bounds from a list of bounds, that is, bounds that are always true such
 *  that a type variable is a subtype of the top type or a supertype of the bottom type.
 */
def removeImplicitBounds(bounds: List[Bound]): List[Bound] =
  bounds.flatMap(bound =>
    (bound.dir, bound.type_) match
      case (Direction.Sub, TTop) | (Direction.Super, TBot) =>
        None
      case _ =>
        Some(bound)
  )
