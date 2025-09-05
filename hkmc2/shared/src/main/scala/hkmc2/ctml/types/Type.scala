package hkmc2.ctml.types

import hkmc2.ctml.util.*
import hkmc2.ctml.util.given

/** A type. */
sealed trait Type:
  /** Get the string representation of the object. */
  override def toString: String =
    showType(this)(using TypePrinter())

/** The bottom type. */
case object TBot extends Type

/** The top type. */
case object TTop extends Type

/** A type variable type. */
case class TVar(val var_ : TypeVar) extends Type

/** A tuple typle. */
case class TTuple(val left: Type, val right: Type) extends Type

/** A lambda type. */
case class TLam(val param: Type, val ret: Type) extends Type

/** A union type. */
case class TUnion(val left: Type, val right: Type) extends Type

/** An intersection type. */
case class TInter(val left: Type, val right: Type) extends Type

/** A type application. */
case class TApp(val abs: Type, val arg: Type) extends Type

/** A universal quantification type. */
case class TUniv(val var_ : TypeVar, val body: Type) extends Type

/** A constrained type. */
case class TConstrained(val body: Type, val bounds: List[Bound]) extends Type

/** A constraining type. */
case class TConstraining(val body: Type, val bounds: List[Bound]) extends Type

/** The top type type alias. */
type TBot = TBot.type

/** The bottom type type alias. */
type TTop = TTop.type

extension (type_ : Type)
  /** Get the components of a type. */
  def components: List[Type] =
    type_ match
      case TBot | TTop | TVar(_) =>
        Nil
      case TTuple(left, right) =>
        List(left, right)
      case TLam(param, ret) =>
        List(param, ret)
      case TUnion(left, right) =>
        List(left, right)
      case TInter(left, right) =>
        List(left, right)
      case TApp(abs, arg) =>
        List(abs, arg)
      case TUniv(_, body) =>
        List(body)
      case TConstrained(body, bounds) =>
        bounds.map(_.type_) :+ body
      case TConstraining(body, bounds) =>
        bounds.map(_.type_) :+ body

/** Implementation of the `Tree` trait for `Type`. */
given Tree[Type] with
  override def children(type_ : Type): List[Type] =
    type_.components

/** Implementation of the `Show` trait for `Type`. */
given Show[Type] with
  override def show(type_ : Type): String =
    showType(type_)(using TypePrinter())
