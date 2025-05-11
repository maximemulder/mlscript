package hkmc2.ctml.types

/** An expression. */
sealed trait Expr

/** A Variable expression. */
class EVar(val name: String) extends Expr

/** A lambda abstraction. */
class ELam(val paramName: String, val body: Expr) extends Expr

/** A lambda application. */
class EApp(val lam: Expr, val arg: Expr) extends Expr

/** A type ascription. */
class EAscr(val expr: Expr, val type_ : Type) extends Expr

// TODO: Add match expression.
