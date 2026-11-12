package hkmc2.ctml.core.subtyping

import hkmc2.ctml.config.config
import hkmc2.ctml.core.makeJointType
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.type_.impls.substitute.substitute
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

/** A local existential problem after replacing `a` by `μX. (F(X) ∨ b)`.
 *  The fresh parameter `b` ranges over all seeds, preserving every solution of `F(a) ≤ a`.
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

/** Parameterize the positive lower self bounds of one locally owned flexible variable.
 *  The ambient context must not mention it. The caller supplies its entire remaining problem,
 *  including its result, so substitution cannot leave an independent copy of the old variable.
 *  Other guards, including upper bounds and negative occurrences, are retained and substituted.
 *  `None` means unsupported, not inconsistent. See `recursive-closure-experiment.md` for the proof.
 */
def closeRecursiveVariable(
  local: TypeVarDecl,
  result: Type,
  constraints: List[Constraint],
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

  /** Read a direct lower bound, irrespective of its written orientation. */
  def lower(guard: Constraint): Option[Type] =
    val (sub, sup) = guard.dir match
      case Direction.Sub => (guard.left, guard.right)
      case Direction.Super => (guard.right, guard.left)
    if sup != TVar(variable) then None
    else
      val body = normalizeRecursiveBound(variable, Direction.Super, sub)
      Option.when(hasRecursiveVariance(body, variable))(body)

  val selected = constraints.map(lower)
  val bodies = selected.flatten
  if !bodies.exists(_.hasVar(variable)) then return None
  val body = bodies.reduce((left, right) => makeJointType(JointMode.Union, left, right))
  val parameter = ctx.mapLevel(_ => local.level).declFreshVar(TypeVarKind.Flex, variable)
  val carrier = newFreshVar()
  val layer = makeJointType(JointMode.Union,
    body.substitute(variable, TVar(carrier)), TVar(parameter.var_))
  val witness = TUniv(carrier,
    TConstrained(TVar(carrier), Constraint(layer, Direction.Sub, TVar(carrier))))
  val remaining = constraints.zip(selected).collect {
    case (guard, None) => guard.substitute(variable, witness)
  }
  Some(RecursiveClosure(parameter, witness, result.substitute(variable, witness), remaining))

/** Solve a freshly opened instance, optionally closing its single local recursive variable.
 *  The scope is complete here: the variable is absent from the target and the ambient context.
 *  Nested comparisons of the synthesized encoding use ordinary instantiation, so the encoding
 *  cannot trigger an unbounded sequence of fresh closure transformations.
 */
private[subtyping] def subtypeRecursiveInstance(sub: Type, target: Type)
    (using ctx: SubContext): Option[SubClauses] =
  if !config.recursiveClosure || ctx.trail.closingRecursiveInstance then return None
  sub match
    case TUniv(variable, body) =>
      val opening = ctx.mapLevel(_ + 1).mapTrail(trail => trail.copy(
        flow = trail.flow + variable.provenance.root, closingRecursiveInstance = true))
      val local = opening.declFreshVar(TypeVarKind.Flex, variable)
      val (result, guards) = body.substitute(variable, TVar(local.var_)).getConstrainedComponents
      closeRecursiveVariable(local, result, guards)(using opening).map(closed =>
        subtypeSeq(closed.guardedResult, target,
          SubClauses.single(closed.parameter))(using opening))
    case _ => None
