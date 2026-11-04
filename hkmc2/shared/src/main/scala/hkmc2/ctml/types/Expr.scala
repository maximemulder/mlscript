package hkmc2.ctml.types

/** An expression. */
sealed trait Expr

/** The unit value, independent of term bindings named `unit`. */
case object EUnit extends Expr

/** A Variable expression. */
case class EVar(val variable: TermVar) extends Expr

/** A tuple introduction */
case class ETuple(val left: Expr, val right: Expr) extends Expr

/** A lambda abstraction. An absent parameter represents a zero-argument function with domain Unit. */
case class ELam(val param: Option[LambdaParam], val body: Expr) extends Expr

/** A lambda parameter with an optional domain annotation. */
case class LambdaParam(variable: TermVar, annotation: Option[Type])

/** A nonrecursive binding or a mutually recursive group, followed by its scope. */
case class ELet(group: BindingGroup, body: Expr) extends Expr

/** A lambda application. */
case class EApp(val lam: Expr, val arg: Expr) extends Expr

/** A type ascription. */
case class EAscr(val expr: Expr, val type_ : Type) extends Expr

/** A pattern matching expression. */
case class EMatch(val scrutinee: Expr, val pattern: Type, val then_ : Expr, val else_ : Option[Expr]) extends Expr
