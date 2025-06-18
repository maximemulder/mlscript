package hkmc2.ctml.types

/** Type alias for wrapped and unwrapped clauses. */
type AsClauses2 = Clauses | List[Clause] | Clause

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
  /** The empty set of clauses. */
  def none =
    Clauses(Nil)

/** A typing clause, which gives a single information about types. */
sealed trait Clause:
  /** Get the clause as a singleton list of clauses. */
  def asClauses: Clauses =
    Clauses(List(this))

  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()

/** A term variable declaration. */
case class TermVarDecl(
  /** The term variable name. */
  name: String,
  /** The term variable type. */
  type_ : Type,
) extends Clause:
  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()

/** A type variable declaration. */
case class TypeVarDecl(
  /** The type variable. */
  var_ : TVar,
  /** The type variable kind. */
  kind: TypeVarKind,
) extends Clause:
  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()

/** A type variable bound. */
case class Bound(
  /** The type variable being bound. */
  var var_ : TVar,
  /** The direction in which the type variable is bound.*/
  var dir: Direction,
  /** The type that bounds the type variable. */
  var type_ : Type,
) extends Clause:
  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()

/** A type variable kind. */
enum TypeVarKind:
  /** A class, which is disjoint with other classes. */
  case Class
  /** A rigid type variable, whose bounds cannot be refined during type checking. */
  case Rigid
  /** A rigid type variable, whose bounds may be refined during type checking. */
  case Fresh

  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()
