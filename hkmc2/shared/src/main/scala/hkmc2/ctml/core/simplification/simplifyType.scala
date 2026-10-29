package hkmc2.ctml.core.simplification

import scala.collection.immutable.LazyList.cons

import hkmc2.ctml.core.*
import hkmc2.ctml.core.type_.impls.getAllVarPolarities.getAllVarPolarities
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.inference.inlineVar
import hkmc2.ctml.core.structural.isIndirectRecursive
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.impls.inline.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

/** Implementation of semantic type simplification. */
extension (type_ : Type)
  /** Simplify the type based on the information available in a context. */
  def simplify()(using ctx: SubContext, noInlineVars: NoInlineVars): Type =
    type_ match
      case TBot | TTop | TVar(_) | TClass(_) =>
        type_
      case TNeg(body) =>
        simplifyNegation(
          body.simplify()
        )
      case TTuple(left, right) =>
        TTuple(
          left.simplify(),
          right.simplify(),
        )
      case TLam(param, ret) =>
        simplifyLambda(
          param.simplify(),
          ret.simplify(),
        )
      case TJointType(mode, left, right) =>
        redundantJointOperand(mode, left, right).getOrElse(simplifyJoint(
          mode,
          left.simplify(),
          right.simplify(),
        ))
      case TApp(abs, arg) =>
        TApp(
          abs.simplify(),
          arg.simplify(),
        )
      case TUniv(var_, body) =>
        val newBody = body.simplify()(using ctx.declTypeVar(var_, TypeVarKind.Flex), noInlineVars)
        simplifyUniv(var_, newBody)
      case TConstrained(body, constraint) =>
        val constraintClauses = try
          subtypeConstraint(constraint)(using ctx)
        catch
          case _: TypeError =>
            SubClauses.empty

        simplifyConstrained(
          body.simplify()(using ctx.extend(constraintClauses)),
          constraint.simplify(),
        )

/** Simplify a negation after its body has already been simplified. */
def simplifyNegation(body: Type): Type =
  makeNegationType(body)

/** Simplify a lambda after its parameter and return type have already been simplified. */
def simplifyLambda(param: Type, ret: Type): Type =
  makeLambdaType(param, ret)

/** Simplify a join or meet after both operands have already been simplified. */
def simplifyJoint(mode: JointMode, left: Type, right: Type)(using ctx: SubContext): Type =
  redundantJointOperand(mode, left, right)
    .getOrElse(hkmc2.ctml.core.combine.combine(mode, left, right))

/** Remove an operand that is an explicit bound of the other operand. Reading the bound directly
 *  also works when subtyping search decomposes its constrained types before looking up the bound.
 *  Check before simplifying the operands, since that can change the shape of the bound. */
private def redundantJointOperand(
  mode: JointMode,
  left: Type,
  right: Type,
)(using ctx: SubContext): Option[Type] =
  val dir = mode match
    case JointMode.Inter => Direction.Sub
    case JointMode.Union => Direction.Super
  (left, right) match
    case (TVar(var_), _) if var_.bound(dir) == right => Some(left)
    case (_, TVar(var_)) if var_.bound(dir) == left => Some(right)
    case _ => None

/** Simplify a universal type after its parameter and return type have already been simplified. */
def simplifyUniv(var_ : TypeVar, body: Type)(using ctx: SubContext, noInlineVars: NoInlineVars): Type =
  if noInlineVars.contains(var_) then
    return TUniv(var_, body)

  body.getInlinePolarities(var_) match
    case Some(polarities) =>
      body.inline(var_)
    case None =>
      TUniv(var_, body)

/** Simplify a constrained type after its body and constraint have already been simplified. */
def simplifyConstrained(body: Type, constraint: Constraint)(using ctx: SubContext): Type =
  if checkConstraint(constraint) then
    body
  else
    makeConstrainedType(body, List(constraint))

extension (constraint: Constraint)
  def simplify()(using ctx: SubContext, noInlineVars: NoInlineVars): Constraint =
    Constraint(
      constraint.left.simplify(),
      constraint.dir,
      constraint.right.simplify(),
    )
