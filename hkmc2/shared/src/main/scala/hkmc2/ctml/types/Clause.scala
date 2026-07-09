package hkmc2.ctml.types

import hkmc2.ctml.utils.*

/** Type alias for clauses-like objects. */
type AsSubClauses = SubContext | SubClauses | List[SubClause] | SubClause

extension (clauses: AsSubClauses)
  /** Read a clauses-like object as a list of clauses. */
  def asSubClauses: List[SubClause] =
    clauses match
      case SubContext(clauses, _, _) =>
        clauses
      case SubClauses(clauses) =>
        clauses
      case clauses: List[SubClause] =>
        clauses
      case clause: SubClause =>
        List(clause)

/** A list of subtyping clauses, which can either be an input subtyping context fragment or output
 *  constraints for a typing or subtyping function. */
case class SubClauses(
  /** The list of clauses itself. */
  elems: List[SubClause] = Nil,
):
  /** Get the string representation of the object. */
  override def toString: String =
    this.show

  /** Concatenate other clauses at the end of these clauses. */
  def concat(others: SubClauses): SubClauses =
    SubClauses(others.elems ::: this.elems)

object SubClauses:
  /** The empty set of clauses. */
  def empty =
    SubClauses(Nil)

  /** A single clause. */
  def single(clause: SubClause) =
    SubClauses(List(clause))

/** A typing context clause. */
sealed trait TypeClause:
  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** A subtyping clause, which gives a single type-level fact. */
sealed trait SubClause extends TypeClause:
  /** Get the clause as a singleton list of subtyping clauses. */
  def asSubClauses: SubClauses =
    SubClauses(List(this))

/** A term variable declaration. */
case class TermVarDecl(
  /** The term variable name. */
  name: String,
  /** The term variable type. */
  type_ : Type,
) extends TypeClause:
  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** A class declaration. */
case class ClassDecl(
  /** The name of the class. */
  name: String,
  /** The parent of the class. */
  parent: Option[ClassVar],
) extends SubClause:
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
  origin: Option[TypeVar],
  /** The level of the type variable. */
  level: Int,
) extends SubClause:
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
) extends SubClause:
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

/** Implementation of the `Show` trait for `SubClauses`. */
given Show[SubClauses] with
  override def show(clauses: SubClauses): String =
    clauses.elems match
      case Nil =>
        "∅"
      case elems =>
        elems.map(_.show).mkString(", ")

/** Implementation of the `Show` trait for `TypeClause`. */
given Show[TypeClause] with
  override def show(clause: TypeClause): String =
    clause match
      case decl: TermVarDecl =>
        decl.show
      case decl: ClassDecl =>
        decl.show
      case decl: TypeVarDecl =>
        decl.show
      case bound: Bound =>
        bound.show

/** Implementation of the `Show` trait for `SubClause`. */
given Show[SubClause] with
  override def show(clause: SubClause): String =
    clause match
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
    s"${var_.var_} ${var_.kind} (${var_.level})"

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
