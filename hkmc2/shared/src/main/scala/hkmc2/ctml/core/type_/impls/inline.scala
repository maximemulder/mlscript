package hkmc2.ctml.core.type_.impls.inline

import hkmc2.ctml.core.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Replace a type variable by a substitute type in a type, simplifying the resulting type if
      possible. */
  def inline(var_ : TypeVar)(using ctx: Context): Type =
    inlineType(type_, var_, Polarity.Positive)

extension (bound: Bound)
  /** Replace a type variable by a substitute type in a bound, simplifying the resulting type if
      possible. */
  def inline(var_ : TypeVar)(using ctx: Context): Bound =
    val newUpper = var_.upperBound.removeDirectVar(bound.var_, Polarity.Negative)
    val newLower = var_.lowerBound.removeDirectVar(bound.var_, Polarity.Positive)
    val newBoundType = inlineType(bound.type_, var_, bound.dir.leftPol)(using
      ctx.extend(
        Bound(var_, Direction.Sub, newUpper),
        Bound(var_, Direction.Super, newLower),
      )
    )
      .removeDirectVar(bound.var_, bound.dir.leftPol)

    Bound(
      bound.var_,
      bound.dir,
      newBoundType,
    )

/** Implementation of semantic type variable inlining. */
private def inlineType(type_ : Type, var_ : TypeVar, pol: Polarity)(using ctx: Context): Type =
  type_ match
    case TVar(typeVar) if typeVar == var_ =>
      var_.bound(pol.dir)
    case TNeg(body) =>
      simplifyNegation(
        inlineType(body, var_, !pol)
      )
    case TTuple(left, right) =>
      val newLeft = inlineType(left, var_, pol)
      val newRight = inlineType(right, var_, pol)
      if newLeft == left && newRight == right then
        type_
      else
        TTuple(newLeft, newRight)
    case TLam(param, ret) =>
      simplifyLambda(
        inlineType(param, var_, !pol),
        inlineType(ret, var_, pol),
      )
    case TJointType(mode, left, right) =>
      val newLeft = inlineType(left, var_, pol)
      val newRight = inlineType(right, var_, pol)
      if newLeft == left && newRight == right then
        type_
      else
        simplifyJoint(mode, newLeft, newRight)
    case TApp(abs, arg) =>
      val newAbs = inlineType(abs, var_, pol)
      val newArg = inlineType(arg, var_, pol)
      if newAbs == abs && newArg == arg then
        type_
      else
        TApp(newAbs, newArg)
    case TUniv(typeVar, _) if typeVar == var_ =>
      type_
    case TUniv(typeVar, body) =>
      val newBody = inlineType(body, var_, pol)(using ctx.declTypeVar(typeVar, TypeVarKind.Rigid))
      simplifyUniv(typeVar, newBody)
    case TConstrained(body, constraint) =>
      val newBody = inlineType(body, var_, pol)
      val newConstraint = inlineConstraint(constraint, var_)
      if newBody == body && newConstraint == constraint then
        type_
      else
        simplifyConstrained(newBody, newConstraint)
    case TConstraining(body, constraint) =>
      val newBody = inlineType(body, var_, pol)
      val newConstraint = inlineConstraint(constraint, var_)
      if newBody == body && newConstraint == constraint then
        type_
      else
        simplifyConstraining(newBody, newConstraint)
    case TBot | TTop | TVar(_) | TClass(_) =>
      type_

/** Inline a variable in a constraint using the current direction-based polarity convention. */
private def inlineConstraint(constraint: Constraint, var_ : TypeVar)(using ctx: Context): Constraint =
  Constraint(
    inlineType(constraint.left, var_, constraint.dir.rightPol),
    constraint.dir,
    inlineType(constraint.right, var_, constraint.dir.leftPol),
  )
