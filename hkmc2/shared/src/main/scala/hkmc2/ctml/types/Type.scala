package hkmc2.ctml.types

import hkmc2.ctml.util.*

/** A type. */
sealed trait Type:
  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()

/** The bottom type. */
case object TBot extends Type

/** The top type. */
case object TTop extends Type

/** A type variable type. */
case class TVar(val var_ : TypeVar) extends Type

/** A lambda type. */
case class TLam(val param: Type, val ret: Type) extends Type

/** A union type. */
case class TUnion(val left: Type, val right: Type) extends Type

/** An intersection type. */
case class TInter(val left: Type, val right: Type) extends Type

/** A constrained type. */
case class TConstrained(val vars: List[TypeVar], val base: Type, val bounds: List[Bound]) extends Type

/** A constraing type. */
case class TConstraining(val base: Type, val bounds: List[Bound]) extends Type

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
      case TLam(param, ret) =>
        List(param, ret)
      case TUnion(left, right) =>
        List(left, right)
      case TInter(left, right) =>
        List(left, right)
      case TConstrained(_, base, bounds) =>
        bounds.map(_.type_) :+ base
      case TConstraining(base, bounds) =>
        bounds.map(_.type_) :+ base

/** The type implementation of the tree trait. */
implicit def TypeTree: Tree[Type] = new Tree[Type] {
  def children(type_ : Type) = type_.components
}
