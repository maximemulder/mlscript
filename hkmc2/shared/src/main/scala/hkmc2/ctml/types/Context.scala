package hkmc2.ctml.types

/** The typing context, which is a collection of variables and bounds. */
case class Context(
  /** The entries of the typing context. */
  entries: List[ContextEntry] = Nil,
):
  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()

/** A typing context entry. */
sealed trait ContextEntry:
  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()

/** A term variable. */
case class TermVar (
  /** The term variable name. */
  name: String,
  /** The term variable type. */
  type_ : Type,
) extends ContextEntry:
  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()

/** A type variable. */
case class TypeVar(
  /** The type variable name. */
  name: String,
  /** The type variable kind. */
  kind: TypeVarKind,
) extends ContextEntry:
  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()

/** The kind of a type variable. */
enum TypeVarKind:
  /** A rigid type variable, whose bounds cannot be refined during type checking. */
  case Rigid
  /** A rigid type variable, whose bounds may be refined during type checking. */
  case Fresh

  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()

/** A type variable bound. */
class Bound(
  /** The name of the type variable being bound. */
  var name: String,
  /** The direction in which the type variable is bound.*/
  var dir: Direction,
  /** The type that bounds the type variable. */
  var type_ : Type,
) extends ContextEntry:
  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()
