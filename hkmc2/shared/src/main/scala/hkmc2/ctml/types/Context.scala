package hkmc2.ctml.types

/** The typing context, which is a collection of variables and bounds. */
type Context = List[CtxLevel]

object Context:
  /** The context with the primitive type variables of the language. */
  val primitive: Context = List(
    CtxVar("Unit",    TVar("Unit")),
    CtxVar("Bool",    TVar("Bool")),
    CtxVar("Int",     TVar("Int")),
    CtxVar("Decimal", TVar("Decimal")),
    CtxVar("String",  TVar("String")),
    CtxTypeVar("Unit",    TypeVarKind.Rigid),
    CtxTypeVar("Bool",    TypeVarKind.Rigid),
    CtxTypeVar("Int",     TypeVarKind.Rigid),
    CtxTypeVar("Decimal", TypeVarKind.Rigid),
    CtxTypeVar("String",  TypeVarKind.Rigid),
  )

  /** The empty context. */
  val empty: Context = Nil

/** A context level. */
sealed class CtxLevel

/** A term variable. */
case class CtxVar(val name: String, val type_ : Type) extends CtxLevel

/** A type variable, which can be either rigid or fresh. */
case class CtxTypeVar(val name: String, val kind: TypeVarKind) extends CtxLevel

/** A type variable bound, which should be respected by any term typed in this context. */
case class CtxBound(val bound: Bound) extends CtxLevel

/** The kind of a type variable. */
enum TypeVarKind:
  /** A rigid type variables, whose bounds cannot be refined during type checking. */
  case Rigid
  /** A rigid type variables, whose bounds may be refined during type checking. */
  case Fresh
