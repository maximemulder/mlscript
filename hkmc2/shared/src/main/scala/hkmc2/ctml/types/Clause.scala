package hkmc2.ctml.types

import hkmc2.ctml.util.*

/** Type alias for clauses-like objects. */
type AsClauses = Context | Clauses | List[Clause] | Clause

extension (clauses: AsClauses)
  /** Read a clauses-like object as a list of clauses. */
  def asClauses: List[Clause] =
    clauses match
      case Context(clauses) =>
        clauses
      case Clauses(clauses) =>
        clauses
      case clauses: List[Clause] =>
        clauses
      case clause: Clause =>
        List(clause)

/** A list of typing clauses, which can either be an input (context) or output (constraints) for a
 *  typing function. */
case class Clauses(
  /** The list of clauses itself. */
  elems: List[Clause] = Nil,
):
  /** Get the string representation of the object. */
  override def toString: String =
    this.show

object Clauses:
  /** The empty set of clauses. */
  def empty =
    Clauses(Nil)

/** A typing clause, which gives a single information about types. */
sealed trait Clause:
  /** Get the clause as a singleton list of clauses. */
  def asClauses: Clauses =
    Clauses(List(this))

  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** A term variable declaration. */
case class TermVarDecl(
  /** The term variable name. */
  name: String,
  /** The term variable type. */
  type_ : Type,
) extends Clause:
  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** A type variable declaration. */
case class TypeVarDecl(
  /** The type variable. */
  var_ : TypeVar,
  /** The type variable kind. */
  kind: TypeVarKind,
) extends Clause:
  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** A type variable bound. */
case class Bound(
  /** The type variable being bound. */
  var var_ : TypeVar,
  /** The direction in which the type variable is bound.*/
  var dir: Direction,
  /** The type that bounds the type variable. */
  var type_ : Type,
) extends Clause:
  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** A type variable kind. */
enum TypeVarKind:
  /** A class, which is disjoint with other classes. */
  case Class
  /** A rigid type variable, whose bounds cannot be refined during type checking. */
  case Rigid
  /** A rigid type variable, whose bounds may be refined during type checking. */
  case Fresh

  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** Implementation of the `Show` trait for `Clause`. */
given Show[Clauses] with
  override def show(clauses: Clauses): String =
    clauses.elems match
      case Nil =>
        "∅"
      case elems =>
        elems.map(_.show).mkString(", ")

/** Implementation of the `Show` trait for `Clause`. */
given Show[Clause] with
  override def show(clause: Clause): String =
    clause match
      case var_ : TermVarDecl =>
        var_.show
      case var_ : TypeVarDecl =>
        var_.show
      case bound: Bound =>
        bound.show

/** Implementation of the `Show` trait for `TermVarDecl`. */
given Show[TermVarDecl] with
  override def show(var_ : TermVarDecl): String =
    s"${var_.name}: ${var_.type_}"

/** Implementation of the `Show` trait for `TypeVarDecl`. */
given Show[TypeVarDecl] with
  override def show(var_ : TypeVarDecl): String =
    s"${var_.var_} ${var_.kind}"

/** Implementation of the `Show` trait for `Bound`. */
given Show[Bound] with
  override def show(bound: Bound): String =
    s"${bound.var_} ${bound.dir} ${bound.type_}"

/** Implementation of the `Show` trait for `TypeVarKind`. */
given Show[TypeVarKind] with
  override def show(kind: TypeVarKind): String =
    kind match
      case TypeVarKind.Class => "class"
      case TypeVarKind.Rigid => "rigid"
      case TypeVarKind.Fresh => "fresh"

/** Convert a list of bounds to its string representation. */
def showBounds(bounds: List[Bound]): String =
  bounds.reverse.map(_.show).mkString(", ")
