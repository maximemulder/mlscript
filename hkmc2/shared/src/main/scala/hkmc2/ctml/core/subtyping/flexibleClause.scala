package hkmc2.ctml.core.subtyping

import hkmc2.ctml.core.makeJointType
import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*

/** Express a Boolean clause as an equivalent bound on its newest exposed flexible operand.
 *  `(a ∧ P) ≤ Q` iff `a ≤ ¬P ∨ Q`, and `P ≤ (a ∨ Q)` iff `P ∧ ¬Q ≤ a`.
 *  Complemented variable operands use contraposition. Other operands remain opaque, so the
 *  transformation neither distributes Boolean expressions nor opens structural types or binders.
 *  The returned constraint preserves the whole solution family; its opposite bounds and scope
 *  still require the ordinary solver checks.
 */
def isolateFlexibleClause(sub: Type, sup: Type)(using ctx: SubContext): Option[Constraint] =
  /** Flatten a single associative connective, retaining all other types as operands. */
  def operands(type_ : Type, mode: JointMode): List[Type] = type_ match
    case TJointType(`mode`, left, right) => operands(left, mode) ::: operands(right, mode)
    case _ => List(type_)

  val left = operands(sub, JointMode.Inter)
  val right = operands(sup, JointMode.Union)
  val variables = (left ::: right).flatMap(_.lookupVar).toSet
  if variables.isEmpty then return None
  // Declaration order breaks ties in favor of the newest variable at the same level.
  // Variable lookup uses the newest declaration, so a rigid shadow must hide an older flex.
  val eligible = ctx.typeVarDecls.filter(d => variables(d.var_)).toList
    .distinctBy(_.var_).filter(_.kind == TypeVarKind.Flex)
  val chosen = eligible.sortBy(d => -d.level).headOption
  chosen.map: declaration =>
    val variable = declaration.var_
    val leftIndex = left.indexWhere(_.lookupVar.contains(variable))
    val (parts, index, direction, opposite) = if leftIndex >= 0 then
      (left, leftIndex, Direction.Sub, sup)
    else
      (right, right.indexWhere(_.lookupVar.contains(variable)), Direction.Super, sub)
    assert(index >= 0, "the selected variable must be an exposed operand")
    val remainder = parts.take(index) ::: parts.drop(index + 1)
    val mode = if direction == Direction.Sub then JointMode.Inter else JointMode.Union
    val identity = if direction == Direction.Sub then TTop else TBot
    val rest = remainder.foldRight(identity: Type)((part, result) => makeJointType(mode, part, result))
    val bound = if direction == Direction.Sub then
      makeJointType(JointMode.Union, rest.negate(), opposite)
    else
      makeJointType(JointMode.Inter, opposite, rest.negate())
    parts(index) match
      case TVar(_) => Constraint(TVar(variable), direction, bound)
      case TNeg(TVar(_)) => Constraint(TVar(variable), !direction, bound.negate())
      case _ => throw IllegalStateException("an exposed variable operand has an unsupported shape")
