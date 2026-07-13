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
        simplifyJoint(
          mode,
          left.simplify(),
          right.simplify(),
        )
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
          subtypeConstraint(constraint)(using ctx, ConstraintMode.Solve)
        catch
          case _: TypeError =>
            SubClauses.empty

        simplifyConstrained(
          body.simplify()(using ctx.extend(constraintClauses)),
          constraint.simplify(),
        )
      case TConstraining(body, constraint) =>
        simplifyConstraining(
          body.simplify(),
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
  hkmc2.ctml.core.combine.combine(mode, left, right)

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

/** Simplify a constraining type after its body and constraint have already been simplified. */
def simplifyConstraining(body: Type, constraint: Constraint)(using ctx: SubContext): Type =
  if checkConstraint(constraint) then
    body
  else
    makeConstrainingType(body, List(constraint))

extension (constraint: Constraint)
  def simplify()(using ctx: SubContext, noInlineVars: NoInlineVars): Constraint =
    Constraint(
      constraint.left.simplify(),
      constraint.dir,
      constraint.right.simplify(),
    )

// Debug function. Should be eventually removed.
extension (constraint: Constraint)
  def asBound: Bound =
    constraint match
      case Constraint(TVar(var_), dir, type_) =>
        Bound(var_, dir, type_)
      case Constraint(type_, dir, TVar(var_)) =>
        Bound(var_, !dir, type_)
      case Constraint(_, _, _) =>
        throw Exception(s"Found unsupported non-bound constraint ${constraint}")
