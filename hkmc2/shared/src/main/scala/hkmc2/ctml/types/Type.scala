package hkmc2.ctml.types

/** A type. */
sealed trait Type

/** The bottom type. */
object TBot extends Type

/** The top type. */
object TTop extends Type

/** A type variable type. */
class TVar(val name: String) extends Type

/** A function type. */
class TFun(val param: Type, val body: Type) extends Type

/** A union type. */
class TUnion(val left: Type, val right: Type) extends Type

/** An intersection type. */
class TInter(val left: Type, val right: Type) extends Type

/** A constraing type. */
class TConstraining(val base: Type, val bounds: List[Bound]) extends Type

type TBot = TBot.type
type TTop = TTop.type
