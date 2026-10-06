package hkmc2.ctml.types

import hkmc2.ctml.utils.*

/** Type alias for clauses-like objects. */
type AsSubClauses = SubContext | SubClauses | List[SubClause] | SubClause

extension (clauses: AsSubClauses)
  /** Read a clauses-like object as a list of clauses. */
  def asSubClauses: List[SubClause] =
    clauses match
      case SubContext(clauses, _, _, _) =>
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

/** A type variable bound.
 *
 *  A type variable may have several bounds in a given direction, whose kinds determine how they
 *  combine into the bound of the variable (see `BoundKind`). Only the operations on bounds defined
 *  in `core/clauses/bounds.scala` depend on the kind of a bound, so that the other operations handle
 *  all bounds uniformly. */
case class Bound(
  /** The type variable being bound. */
  val var_ : TypeVar,
  /** The direction in which the type variable is bound.*/
  val dir: Direction,
  /** The type that bounds the type variable. */
  val type_ : Type,
  /** The kind of the bound. */
  val kind: BoundKind,
) extends SubClause:
  /** Get the string representation of the object. */
  override def toString: String =
    this.show

  /** Convert this bound to a constraint. */
  def toConstraint: Constraint =
    Constraint(TVar(var_), dir, type_)

/** The kind of a type variable bound. */
enum BoundKind:
  /** An asserted bound, which holds because of the constraint it originates from.
   *
   *  All the asserted bounds of a variable in a given direction hold, so that its bound in that
   *  direction is their combination (see `varBound`). Asserted bounds are never replaced, which
   *  makes them independent of the scope they are computed in: they are the bounds that are output
   *  by constraint solving and joined by context joins. */
  case Asserted

  /** An effective bound, which subsumes the bounds of its variable in the same direction that
   *  precede it, in the scope in which it is computed.
   *
   *  When reading the bound of a type variable, the bounds that precede its most recent effective
   *  bound are thus ignored (see `varBound`). This allows the bound of a variable to be simplified
   *  in a scope without losing the asserted bounds it is computed from. Notably, the combination of
   *  the current bound of a variable with a new bound is recorded as an effective bound (see
   *  `subtypeFlexVar`), so that it does not need to be recomputed whenever the variable is used.
   *
   *  Since an effective bound may rely on the assumptions of the scope in which it is computed (the
   *  guard of a constrained type, the branch of a match...), effective bounds are removed from the
   *  clauses output by a computation when they leave that scope (see `removeEffectiveBounds`), which
   *  leaves the asserted bounds. Losing an effective bound is harmless: the bound of the variable is
   *  then read as the combination of its asserted bounds.
   *
   *  The asserted bounds of a variable are kept alongside its effective bounds rather than replaced
   *  by them. Replacing them requires every new bound to subsume the previous ones, which the
   *  bounds produced by context joins do not: e.g. the join of nested matches would then lose the
   *  bound of the outer pattern, so that `foo(1, "World")` would be accepted in
   *  `ctmlFlowWeirdMatch.mls`. */
  case Effective

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
    bound.kind match
      case BoundKind.Asserted =>
        s"${bound.var_} ${bound.dir} ${bound.type_}"
      case BoundKind.Effective =>
        s"eff ${bound.var_} ${bound.dir} ${bound.type_}"

/** Implementation of the `Show` trait for `TypeVarKind`. */
given Show[TypeVarKind] with
  override def show(kind: TypeVarKind): String =
    kind match
      case TypeVarKind.Rigid => "rigid"
      case TypeVarKind.Flex  => "flex"

/** An unsolved clause. */
enum UnsolvedClause:
  case Var(val var_ : TypeVar)
  case Constr(val constraint: Constraint)

/** A list of unsolved clause. */
class UnsolvedClauses(val elems: List[UnsolvedClause]):
  def ::(clause: UnsolvedClause): UnsolvedClauses =
    UnsolvedClauses(clause :: this.elems)

object UnsolvedClauses:
  def empty = UnsolvedClauses(List())
