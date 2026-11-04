package hkmc2.ctml.parser

import scala.collection.mutable
import hkmc2.asInt
import hkmc2.ctml.types.*
import hkmc2.semantics.{DefineVar, LetDecl, Statement, Symbol, Term, TermDefinition}
import hkmc2.syntax.Fun

/** Preserve the elaborator's variable identity across nested and reordered definitions. */
def termVar(symbol: Symbol): TermVar = TermVar(symbol.nme, Some(symbol.uid.asInt))

/** Whether a statement belongs to an immutable binding block. */
def isBindingStatement(stmt: Statement): Boolean = stmt match
  case definition: TermDefinition => definition.body.isDefined
  case _: DefineVar | _: LetDecl => true
  case _ => false

/** Translate a binding block, preserving value initialization order while allowing function
 *  closures to be checked before their users and after the values that they capture. */
def parseBindingGroups(stmts: List[Statement])(using Scope): List[BindingGroup] =
  require(stmts.forall(isBindingStatement))
  val sources = stmts.filterNot(_.isInstanceOf[LetDecl])
  val bindings = sources.map(parseBinding)
  val values = sources.zip(bindings).flatMap:
    case (definition: TermDefinition, binding) if definition.k == Fun && binding.expr.isFunction =>
      Nil
    case (_, binding) => List(binding.variable)
  orderBindings(bindings, values)

/** Translate a definition, including its parameter and result annotations. */
def parseBinding(stmt: Statement)(using Scope): Binding = stmt match
  case DefineVar(symbol, expr) => Binding(termVar(symbol), parseExpr(expr))
  case definition: TermDefinition =>
    if definition.tparams.exists(_.nonEmpty) then
      throw ParseError(definition, Some("Type parameters on function definitions are unsupported."))
    val expr = parseExpr(definition.body.getOrElse(throw ParseError(definition)))
    val body = definition.sign.fold(expr)(sign => EAscr(expr, parseType(sign)))
    val function = definition.params.foldRight(body)(wrapLambda(_, _))
    Binding(termVar(definition.sym), function)
  case _ => throw ParseError(stmt)

/** Partition bindings into strongly connected components, with dependencies before their users.
 *  Values additionally depend on the preceding value, so their evaluation order cannot change.
 *  Only closures may form cycles: a cycle containing a computation needs an initialization
 *  semantics beyond immutable bindings. Traversal and component ordering are deterministic. */
private def orderBindings(bindings: List[Binding], values: List[TermVar]): List[BindingGroup] =
  val variables = bindings.map(_.variable).toSet
  if variables.size != bindings.size then
    throw ParseError(Term.Error(), Some("Multiple definitions of one binding are unsupported."))
  val previousValues = values.drop(1).zip(values).toMap
  val dependencies = bindings.map(binding =>
    binding.variable -> (binding.expr.freeVars.intersect(variables) ++
      previousValues.get(binding.variable))
  ).toMap
  val indices = mutable.Map.empty[TermVar, Int]
  val low = mutable.Map.empty[TermVar, Int]
  val active = mutable.Set.empty[TermVar]
  var stack = List.empty[TermVar]
  val groups = mutable.ListBuffer.empty[BindingGroup]

  /** Visit a binding and emit its component after all components on which it depends. */
  def visit(variable: TermVar): Unit =
    val index = indices.size
    indices(variable) = index
    low(variable) = index
    stack = variable :: stack
    active += variable
    for dependency <- bindings.map(_.variable).filter(dependencies(variable)) do
      if !indices.contains(dependency) then
        visit(dependency)
        low(variable) = low(variable).min(low(dependency))
      else if active(dependency) then
        low(variable) = low(variable).min(indices(dependency))
    if low(variable) == indices(variable) then
      val (prefix, suffix) = stack.span(_ != variable)
      val component = (variable :: prefix).toSet
      stack = suffix.tail
      active --= component
      val members = bindings.filter(binding => component(binding.variable))
      val recursive = members.size > 1 || dependencies(variable)(variable)
      if recursive && !members.forall(_.expr.isFunction) then
        throw ParseError(Term.Error(),
          Some("Recursive bindings require a function initializer."))
      groups += BindingGroup(members, recursive)

  for binding <- bindings if !indices.contains(binding.variable) do visit(binding.variable)
  groups.toList
