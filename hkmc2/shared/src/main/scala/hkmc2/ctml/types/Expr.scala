package hkmc2.ctml.types

/** An expression. */
sealed trait Expr

/** A Variable expression. */
case class EVar(val name: String) extends Expr

/** A lambda abstraction. */
case class ELam(val paramName: String, val body: Expr) extends Expr

/** A lambda application. */
case class EApp(val lam: Expr, val arg: Expr) extends Expr

/** A type ascription. */
case class EAscr(val expr: Expr, val type_ : Type) extends Expr

/** A pattern matching expression. */
case class EMatch(val scrutinee: Expr, val cases: List[EMatchCase]) extends Expr

/** A pattern matching case. */
case class EMatchCase(val pattern: Type, val body: Expr)
