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

/** A lambda application. */
case class EApp(val lam: Expr, val arg: Expr) extends Expr

/** A type ascription. */
case class EAscr(val expr: Expr, val type_ : Type) extends Expr

/** A pattern matching expression. */
case class EMatch(val scrutinee: Expr, val pattern: Type, val then_ : Expr, val else_ : Option[Expr]) extends Expr

extension (expr: Expr)
  /** Whether an initializer creates a closure, possibly with an erased type ascription. */
  def isFunction: Boolean = expr match
    case ELam(_, _) => true
    case EAscr(body, _) => body.isFunction
    case _ => false

  /** The free term variables, retaining the identity of each elaborated binding. */
  def freeVars: Set[TermVar] = expr match
    case EUnit => Set.empty
    case EVar(variable) => Set(variable)
    case ELam(param, body) => body.freeVars -- param.map(_.variable)
    case EApp(lam, arg) => lam.freeVars ++ arg.freeVars
    case ETuple(left, right) => left.freeVars ++ right.freeVars
    case EAscr(expr, _) => expr.freeVars
    case EMatch(scrutinee, _, then_, else_) =>
      scrutinee.freeVars ++ then_.freeVars ++ else_.toSet.flatMap(_.freeVars)
