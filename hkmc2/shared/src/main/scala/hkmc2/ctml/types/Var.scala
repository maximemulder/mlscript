package hkmc2.ctml.types

import hkmc2.ctml.utils.*

/** A term variable. Resolved variables retain their elaborator identity; synthetic references
 *  select a prelude operation by name. */
case class TermVar(name: String, id: Option[Int]):
  /** Display the source name without its internal identity. */
  override def toString: String = name

/** A type variable with immutable search provenance. Concrete equality depends only on the name,
 *  as in the type language; provenance never identifies two independently allocated variables. */
final class TypeVar(val name: String, val provenance: VarProvenance):
  /** Compare concrete identities without consulting the search abstraction. */
  override def equals(other: Any): Boolean = other match
    case other: TypeVar => name == other.name
    case _ => false

  /** Hash the concrete identity, independently of search provenance. */
  override def hashCode(): Int = name.hashCode

  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** Construction and pattern matching for concrete type variables. */
object TypeVar:
  /** Construct a variable whose name is also its initial origin. */
  def apply(name: String): TypeVar =
    new TypeVar(name, VarProvenance(VarOrigin.Named(name), VarRole.Root, Set.empty))

  /** Read the concrete name without erasing the provenance on the variable itself. */
  def unapply(variable: TypeVar): Some[String] = Some(variable.name)

/** A stable creation site. Source symbols distinguish equally named binders; generated names
 *  distinguish inference operations without introducing another global counter. */
enum VarOrigin:
  /** A declaration or inference operation identified by its concrete name. */
  case Named(name: String)
  /** A quantified binder identified by its elaborator symbol. */
  case Source(id: Int)

/** The operation that creates an instance, independently of its current rigid/flexible status. */
enum VarRole:
  /** A source declaration or an inference variable before instantiation. */
  case Root
  /** An opening of a quantified binder. */
  case Open(kind: TypeVarKind)
  /** A directional extrusion of a variable. */
  case Extrude(pol: Polarity)

/** A finite-set abstraction of instantiation ancestry. Alpha-renaming preserves this metadata;
 *  opening and extrusion preserve the root but record their creation role. It is a search hint,
 *  not evidence of equality, and contains neither fresh instance identities nor an ancestry chain. */
case class VarProvenance(root: VarOrigin, role: VarRole, context: Set[VarOrigin])

/** A class variable. */
case class ClassVar(val name: String):
  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** Implementation of the `Show` trait for `TypeVar`. */
given Show[TypeVar] with
  override def show(var_ : TypeVar): String =
    var_.name

/** Implementation of the `Show` trait for `ClassVar`. */
given Show[ClassVar] with
  override def show(var_ : ClassVar): String =
    var_.name
