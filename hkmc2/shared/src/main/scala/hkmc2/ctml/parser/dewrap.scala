package hkmc2.ctml.parser

import hkmc2.ctml.types.*

/** Inline a UCS scrutinee binding when this preserves evaluation order and multiplicity.
 *  A variable, possibly ascribed, can be reused. A computation can only replace the first
 *  scrutinee when that is the binding's sole free occurrence. Other computations keep their
 *  binding so that subsequent tests and branch uses do not evaluate them again. */
private[parser] def dewrapMatchBinding(name: TermVar, arg: Expr, body: Expr): Option[Expr] =
  body match
    case EMatch(EVar(scrutinee), _, _, _) if scrutinee == name =>
      if reusableScrutinee(arg) || freeOccurrences(body, name) == 1 then
        substituteTerm(body, name, arg)
      else None
    case _ => None

/** Whether duplicating a scrutinee only repeats a variable lookup and erased ascriptions. */
private def reusableScrutinee(expr: Expr): Boolean =
  expr match
    case EUnit => true
    case EVar(_) => true
    case EAscr(expr, _) => reusableScrutinee(expr)
    case _ => false

/** Count free occurrences, respecting the shadowing of named lambda parameters. */
private def freeOccurrences(expr: Expr, name: TermVar): Int =
  expr match
    case EUnit => 0
    case EVar(other) => if other == name then 1 else 0
    case ELam(param, body) => if param.exists(_.variable == name) then 0 else freeOccurrences(body, name)
    case ELet(group, body) =>
      val bound = group.variables(name)
      val rhsOccurrences = if bound && group.recursive then 0
        else group.bindings.map(binding => freeOccurrences(binding.expr, name)).sum
      rhsOccurrences + (if bound then 0 else freeOccurrences(body, name))
    case EApp(lam, arg) => freeOccurrences(lam, name) + freeOccurrences(arg, name)
    case ETuple(left, right) => freeOccurrences(left, name) + freeOccurrences(right, name)
    case EAscr(expr, _) => freeOccurrences(expr, name)
    case EMatch(scrutinee, _, then_, else_) =>
      freeOccurrences(scrutinee, name) + freeOccurrences(then_, name) +
        else_.map(freeOccurrences(_, name)).getOrElse(0)

/** Replace a free variable, declining the rewrite if an intervening binder would capture it. */
private def substituteTerm(expr: Expr, name: TermVar, arg: Expr): Option[Expr] =
  expr match
    case EUnit => Some(EUnit)
    case EVar(other) => Some(if other == name then arg else expr)
    case ELam(param, body) =>
      if param.exists(_.variable == name) then Some(expr)
      else if param.exists(p => freeOccurrences(arg, p.variable) > 0) && freeOccurrences(body, name) > 0 then None
      else substituteTerm(body, name, arg).map(ELam(param, _))
    case ELet(group, body) =>
      val bound = group.variables(name)
      val captures = group.variables.exists(variable => freeOccurrences(arg, variable) > 0)
      if captures && freeOccurrences(expr, name) > 0 then None
      else
        val bindings = group.bindings.foldRight(Option(List.empty[Binding])): (binding, rest) =>
          for
            rest <- rest
            rhs <- if bound && group.recursive then Some(binding.expr)
              else substituteTerm(binding.expr, name, arg)
          yield binding.copy(expr = rhs) :: rest
        for
          bindings <- bindings
          body <- if bound then Some(body) else substituteTerm(body, name, arg)
        yield ELet(group.copy(bindings = bindings), body)
    case EApp(lam, value) =>
      for
        lam <- substituteTerm(lam, name, arg)
        value <- substituteTerm(value, name, arg)
      yield EApp(lam, value)
    case ETuple(left, right) =>
      for
        left <- substituteTerm(left, name, arg)
        right <- substituteTerm(right, name, arg)
      yield ETuple(left, right)
    case EAscr(expr, type_) => substituteTerm(expr, name, arg).map(EAscr(_, type_))
    case EMatch(scrutinee, pattern, then_, else_) =>
      for
        scrutinee <- substituteTerm(scrutinee, name, arg)
        then_ <- substituteTerm(then_, name, arg)
        else_ <- else_.fold[Option[Option[Expr]]](Some(None))(
          expr => substituteTerm(expr, name, arg).map(Some(_)))
      yield EMatch(scrutinee, pattern, then_, else_)
