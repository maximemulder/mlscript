package hkmc2.ctml.types

/** A CTML meta-expression. */
sealed trait Meta

/** A standard CTML expression. */
case class MetaExpr(expr: Expr) extends Meta
/** A relation between two types. */
case class MetaType(rel: TypeRel, left: Type, right: Type) extends Meta

/** A relation between two types. */
enum TypeRel:
  /** The two types are equal. */
  case Eq
  /** The two types are not equal. */
  case Ne
  /** The left type is a subtype of the right type. */
  case Sub
  /** The left type is a supertype of the right type. */
  case Sup
