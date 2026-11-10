package hkmc2.ctml.parser

import hkmc2.asInt
import hkmc2.ctml.types.*
import hkmc2.semantics.{DefineVar, Statement, Symbol, TermDefinition}

/** Preserve the elaborator's variable identity across nested definitions. */
def termVar(symbol: Symbol): TermVar = TermVar(symbol.nme, Some(symbol.uid.asInt))

/** Translate a single immutable binding, retaining parameter and result annotations.
 *  Direct recursive functions are expressed using the prelude fixed-point combinator. Its
 *  synthetic reference cannot be captured by a local binding named `fix`. Recursive value
 *  initializers are unsupported because they require an initialization semantics. */
def parseBinding(stmt: Statement)(using Scope): (TermVar, Expr) =
  val (variable, expr) = stmt match
    case DefineVar(symbol, expr) => (termVar(symbol), parseExpr(expr))
    case definition: TermDefinition =>
      if definition.tparams.exists(_.nonEmpty) then
        throw ParseError(definition, Some("Type parameters on function definitions are unsupported."))
      val expr = parseExpr(definition.body.getOrElse(throw ParseError(definition)))
      val body = definition.sign.fold(expr)(sign => EAscr(expr, parseType(sign)))
      (termVar(definition.sym), definition.params.foldRight(body)(wrapLambda(_, _)))
    case _ => throw ParseError(stmt)
  val result = if expr.freeVars(variable) then
    if !expr.isFunction then
      throw ParseError(stmt, Some("Recursive bindings require a function initializer."))
    EApp(EVar(TermVar("fix", None)), ELam(Some(LambdaParam(variable, None)), expr))
  else expr
  (variable, result)
