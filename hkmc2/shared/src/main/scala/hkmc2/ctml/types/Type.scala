package hkmc2.ctml.types

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
case class TVar(val name: String) extends Type

/** A lambda type. */
case class TLam(val param: Type, val ret: Type) extends Type

/** A union type. */
case class TUnion(val left: Type, val right: Type) extends Type

/** An intersection type. */
case class TInter(val left: Type, val right: Type) extends Type

/** A constraing type. */
case class TConstraining(val base: Type, val bounds: List[Bound]) extends Type

type TBot = TBot.type
type TTop = TTop.type
