package hkmc2.ctml.types

import hkmc2.ctml.utils.*

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

  /** Concatenate other clauses at the end of these clauses. */
  def concat(others: Clauses): Clauses =
    Clauses(others.elems ::: this.elems)

object Clauses:
  /** The empty set of clauses. */
  def empty =
    Clauses(Nil)

  /** A single clause. */
  def single(clause: Clause) =
    Clauses(List(clause))

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

/** A class declaration. */
case class ClassDecl(
  /** The name of the class. */
  name: String,
  /** The parent of the class. */
  parent: Option[String],
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
  /** The original type variable if the variable is fresh. */
  original: Option[TypeVar],
  /** The level of the type variable. */
  level: Int,
) extends Clause:
  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** A type variable bound. */
case class Bound(
  /** The type variable being bound. */
  val var_ : TypeVar,
  /** The direction in which the type variable is bound.*/
  val dir: Direction,
  /** The type that bounds the type variable. */
  val type_ : Type,
) extends Clause:
  /** Get the string representation of the object. */
  override def toString: String =
    this.show

  /** Convert this bound to a constraint. */
  def toConstraint: Constraint =
    Constraint(TVar(var_), dir, type_)

/** A type variable kind. */
enum TypeVarKind:
  /** A rigid type variable, whose bounds cannot be refined during type checking. */
  case Rigid
  /** A flexible type variable, whose bounds may be refined during type checking. */
  case Flex

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
      case decl: TermVarDecl =>
        decl.show
      case decl: ClassDecl =>
        decl.show
      case decl: TypeVarDecl =>
        decl.show
      case bound: Bound =>
        bound.show

/** Implementation of the `Show` trait for `TermVarDecl`. */
given Show[TermVarDecl] with
  override def show(var_ : TermVarDecl): String =
    s"${var_.name}: ${var_.type_}"

/** Implementation of the `Show` trait for `ClassDecl`. */
given Show[ClassDecl] with
  override def show(class_ : ClassDecl): String =
    s"class ${class_.name} ${class_.parent}"

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
      case TypeVarKind.Rigid => "rigid"
      case TypeVarKind.Flex  => "flex"
