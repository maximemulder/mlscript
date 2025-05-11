package hkmc2.ctml.types

/** The typing context, which is a collection of variables and bounds. */
type Context = List[CtxLevel]

object Context:
  val empty: Context = List(
    CtxTypeVar("Unit",   TypeVarKind.Rigid),
    CtxTypeVar("Bool",   TypeVarKind.Rigid),
    CtxTypeVar("Int",    TypeVarKind.Rigid),
    CtxTypeVar("Float",  TypeVarKind.Rigid),
    CtxTypeVar("String", TypeVarKind.Rigid),
  )

/** A context level. */
sealed class CtxLevel

/** A term variable. */
class CtxVar(val name: String, val type_ : Type) extends CtxLevel

/** A type variable, which can be either rigid or fresh. */
class CtxTypeVar(val name: String, val kind: TypeVarKind) extends CtxLevel

/** A type variable bound, which should be respected by any term typed in this context. */
class CtxBound(val bound: Bound) extends CtxLevel

/** The kind of a type variable. */
enum TypeVarKind:
  /** A rigid type variables, whose bounds cannot be refined during type checking. */
  case Rigid
  /** A rigid type variables, whose bounds may be refined during type checking. */
  case Fresh
