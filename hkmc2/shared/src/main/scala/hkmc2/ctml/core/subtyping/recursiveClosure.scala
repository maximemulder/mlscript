package hkmc2.ctml.core.subtyping

import hkmc2.ctml.config.config
import hkmc2.ctml.core.{makeJointType, makeNegationType}
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.type_.impls.substitute.substitute
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

/** A local existential problem after parameterizing one monotone recursive bound.
 *  A lower bound uses `μX. (F(X) ∨ b)`; an upper bound uses its complement dual.
 *  The fresh parameter ranges over all seeds, preserving every solution of the selected bound.
 *  Remaining constraints still need solving; constructing a closure is not a satisfiability test.
 */
case class RecursiveClosure(
  parameter: TypeVarDecl,
  witness: Type,
  result: Type,
  constraints: List[Constraint],
):
  /** Reattach the remaining obligations in their original nesting order. */
  def guardedResult: Type =
    constraints.foldRight(result)((guard, body) => TConstrained(body, guard))

/** Certify variance using structural subtyping rules, without consulting variable bounds.
 *  A recursive occurrence beneath a guard or universal is outside this experiment's fragment.
 *  Independent subexpressions may contain either. Applications follow CTML's covariant rule.
 */
private def hasRecursiveVariance(
  body: Type,
  variable: TypeVar,
  positive: Boolean = true,
): Boolean =
  if !body.hasVar(variable) then true
  else body match
    case TVar(other) => other == variable && positive
    case TNeg(inner) => hasRecursiveVariance(inner, variable, !positive)
    case TLam(param, ret) =>
      hasRecursiveVariance(param, variable, !positive) &&
        hasRecursiveVariance(ret, variable, positive)
    case TField(_, value) => hasRecursiveVariance(value, variable, positive)
    case TJointType(_, left, right) =>
      hasRecursiveVariance(left, variable, positive) &&
        hasRecursiveVariance(right, variable, positive)
    case TTuple(left, right) =>
      hasRecursiveVariance(left, variable, positive) &&
        hasRecursiveVariance(right, variable, positive)
    case TApp(abs, arg) =>
      hasRecursiveVariance(abs, variable, positive) &&
        hasRecursiveVariance(arg, variable, positive)
    case _ => false

/** Parameterize monotone self bounds in one direction of a locally owned flexible variable.
 *  The ambient context must not mention it. The caller supplies its entire remaining problem,
 *  including its result, so substitution cannot leave an independent copy of the old variable.
 *  Other guards, including opposite bounds and negative occurrences, are retained and substituted.
 *  `None` means unsupported, not inconsistent. See `recursive-closure-experiment.md` for the proof.
 */
def closeRecursiveVariable(
  local: TypeVarDecl,
  result: Type,
  constraints: List[Constraint],
  direction: Direction = Direction.Super,
)(using ctx: SubContext): Option[RecursiveClosure] =
  if local.kind != TypeVarKind.Flex then return None
  val variable = local.var_
  assert(!ctx.typeVarDecls.exists(_.var_ == variable), "the closed variable must be locally owned")
  assert(!ctx.clauses.exists {
    case Bound(other, _, bound, _) => other == variable || bound.hasVar(variable)
    case _ => false
  }, "ambient bounds must not depend on the closed variable")
  assert(!ctx.hypotheses.judgments.exists((sub, sup) =>
    sub.hasVar(variable) || sup.hasVar(variable)), "ambient assumptions must be independent")

  val free = (result.getVars ++ constraints.flatMap(_.getVars)) - variable
  assert(free.forall(v => ctx.typeVarDecls.exists(_.var_ == v)), "undeclared free variable")
  // A seed at the original level cannot carry a dependency on a later rigid variable.
  if free.exists(_.level > local.level) then return None

  /** Read a direct bound in the selected direction, irrespective of written orientation. */
  def selectedBound(guard: Constraint): Option[Type] =
    val (left, right) = if guard.dir == direction then (guard.left, guard.right)
      else (guard.right, guard.left)
    if left != TVar(variable) then None
    else
      val body = normalizeRecursiveBound(variable, direction, right)
      Option.when(hasRecursiveVariance(body, variable))(body)

  val selected = constraints.map(selectedBound)
  val bodies = selected.flatten
  if !bodies.exists(_.hasVar(variable)) then return None
  val body = bodies.reduce((left, right) => makeJointType(direction.jointMode, left, right))
  val parameter = ctx.mapLevel(_ => local.level).declFreshVar(TypeVarKind.Flex, variable)
  val carrier = newFreshVar()
  val recursiveBody = direction match
    case Direction.Super => body.substitute(variable, TVar(carrier))
    case Direction.Sub =>
      makeNegationType(body.substitute(variable, makeNegationType(TVar(carrier))))
  val layer = makeJointType(JointMode.Union, recursiveBody, TVar(parameter.var_))
  val closure = TUniv(carrier,
    TConstrained(TVar(carrier), Constraint(layer, Direction.Sub, TVar(carrier))))
  val witness = if direction == Direction.Super then closure else makeNegationType(closure)
  val remaining = constraints.zip(selected).collect {
    case (guard, None) => guard.substitute(variable, witness)
  }
  Some(RecursiveClosure(parameter, witness, result.substitute(variable, witness), remaining))

/** A complete local problem after zero or more variables have been parameterized. */
case class RecursiveProblem(
  declarations: List[TypeVarDecl],
  result: Type,
  constraints: List[Constraint],
):
  /** Retain all residual guards without simplifying them as if they were solved bounds. */
  def guardedResult: Type =
    constraints.foldRight(result)((guard, body) => TConstrained(body, guard))

/** Normalize a complete local existential problem, keeping its ambient context independent.
 *  Each substitution reaches the whole remaining problem. Other local declarations are available
 *  as parameters, but none of their bounds are assumed while certifying a transformation.
 *  Each original variable is considered once; seeds are never closed again on this pass.
 *  Lower bounds take priority. An opposite bound remains a substituted obligation.
 */
def closeRecursiveProblem(problem: RecursiveProblem)(using ctx: SubContext): Option[RecursiveProblem] =
  assert(problem.declarations.map(_.var_).distinct.size == problem.declarations.size)
  assert(!problem.declarations.exists(d => ctx.typeVarDecls.exists(_.var_ == d.var_)))
  var current = problem
  var changed = false
  for local <- problem.declarations do
    val independent = ctx.extend(current.declarations.filterNot(_.var_ == local.var_))
    val closed = closeRecursiveVariable(local, current.result, current.constraints)(using independent)
      .orElse(closeRecursiveVariable(local, current.result, current.constraints, Direction.Sub)
        (using independent))
    closed.foreach: closed =>
      current = RecursiveProblem(
        current.declarations.map(d => if d.var_ == local.var_ then closed.parameter else d),
        closed.result, closed.constraints)
      changed = true
  Option.when(changed)(current)

/** Solve a freshly opened instance in a complete scope independent of the target and ambient
 *  context. The algebraic experiment opens all leading binders and supports dual upper closure.
 *  Generated encodings use ordinary instantiation to avoid repeatedly closing their own carriers.
 */
private[subtyping] def subtypeRecursiveInstance(sub: Type, target: Type)
    (using ctx: SubContext): Option[SubClauses] =
  if !config.recursiveClosure || ctx.trail.closingRecursiveInstance then return None
  sub match
    case univ: TUniv =>
      val (variables, body) = if config.algebraicRecursion then univ.getUnivComponents
        else (List(univ.var_), univ.body)
      val opening = ctx.mapLevel(_ + 1).mapTrail(trail => trail.copy(
        flow = trail.flow ++ variables.map(_.provenance.root), closingRecursiveInstance = true))
      val locals = variables.map(opening.declFreshVar(TypeVarKind.Flex, _))
      val instance = variables.zip(locals).foldLeft(body)((body, entry) =>
        body.substitute(entry._1, TVar(entry._2.var_)))
      val (result, guards) = instance.getConstrainedComponents
      val normalized = if config.algebraicRecursion then
        closeRecursiveProblem(RecursiveProblem(locals, result, guards))(using opening)
      else
        closeRecursiveVariable(locals.head, result, guards)(using opening).map(closed =>
          RecursiveProblem(List(closed.parameter), closed.result, closed.constraints))
      normalized.map(closed =>
        subtypeSeq(closed.guardedResult, target, SubClauses(closed.declarations))(using opening))
    case _ => None
