package hkmc2.ctml.types

/** An immutable term binding. */
case class Binding(variable: TermVar, expr: Expr)

/** A single nonrecursive binding or a nonempty recursive group of functions.
 *  Dependency ordering belongs to the parser; inference only sees one component at a time. */
case class BindingGroup(bindings: List[Binding], recursive: Boolean):
  require(bindings.nonEmpty)
  require(recursive || bindings.size == 1)
  require(bindings.map(_.variable).distinct.size == bindings.size)
  require(!recursive || bindings.forall(_.expr.isFunction))

  /** The variables introduced by this group. */
  def variables: Set[TermVar] = bindings.map(_.variable).toSet

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
    case ELet(group, body) =>
      val rhsVars = group.bindings.flatMap(_.expr.freeVars).toSet
      (rhsVars -- (if group.recursive then group.variables else Set.empty)) ++
        (body.freeVars -- group.variables)
    case EApp(lam, arg) => lam.freeVars ++ arg.freeVars
    case ETuple(left, right) => left.freeVars ++ right.freeVars
    case EAscr(expr, _) => expr.freeVars
    case EMatch(scrutinee, _, then_, else_) =>
      scrutinee.freeVars ++ then_.freeVars ++ else_.toSet.flatMap(_.freeVars)
