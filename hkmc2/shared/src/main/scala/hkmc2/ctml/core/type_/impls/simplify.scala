package hkmc2.ctml.core.type_.impls.simplify

import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Simplify the type based on the information available in a context. */
  def simplify()(using ctx: Context): Type =
    simplifyType(type_)

/** Implementation of semantic type simplification. */
private def simplifyType(type_ : Type)(using ctx: Context): Type =
  type_ match
    case TBot | TTop | TVar(_) | TClass(_) =>
      type_
    case TNeg(body) =>
      simplifyNegation(
        simplifyType(body)
      )
    case TTuple(left, right) =>
      TTuple(
        simplifyType(left),
        simplifyType(right),
      )
    case TLam(param, ret) =>
      simplifyLambda(
        simplifyType(param),
        simplifyType(ret),
      )
    case TJointType(mode, left, right) =>
      simplifyJoint(
        mode,
        simplifyType(left),
        simplifyType(right),
      )
    case TApp(abs, arg) =>
      TApp(
        simplifyType(abs),
        simplifyType(arg),
      )
    case TUniv(var_, body) =>
      val newBody = simplifyType(body)(using ctx.declTypeVar(var_, TypeVarKind.Rigid))
      simplifyUniv(var_, newBody)
    case TConstrained(body, constraint) =>
      simplifyConstrained(
        simplifyType(body),
        simplifyConstraint(constraint),
      )
    case TConstraining(body, constraint) =>
      simplifyConstraining(
        simplifyType(body),
        simplifyConstraint(constraint),
      )

private def simplifyConstraint(constraint: Constraint)(using ctx: Context): Constraint =
  Constraint(
    simplifyType(constraint.left),
    constraint.dir,
    simplifyType(constraint.right),
  )
