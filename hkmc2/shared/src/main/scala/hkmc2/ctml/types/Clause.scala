package hkmc2.ctml.types

/** A list of typing clauses, which can either be an input (context) or output (constraints) for a
 *  typing function. */
case class Clauses(
  /** The list of clauses itself. */
  elems: List[Clause] = Nil,
):
  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()

object Clauses:
  def none = Clauses(Nil)

/** A typing clause, which gives a single information about types. */
sealed trait Clause:
  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()

/** A term variable. */
case class TermVar (
  /** The term variable name. */
  name: String,
  /** The term variable type. */
  type_ : Type,
) extends Clause:
  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()

/** A type variable. */
case class TypeVar(
  /** The type variable name. */
  name: String,
  /** The type variable kind. */
  kind: TypeVarKind,
) extends Clause:
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
) extends Clause:
  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()
