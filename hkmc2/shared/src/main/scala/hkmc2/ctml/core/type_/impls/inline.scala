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
    case neg: TNeg =>
      inlineNegation(neg, var_, pol)
    case tuple: TTuple =>
      inlineTuple(tuple, var_, pol)
    case lam: TLam =>
      inlineLambda(lam, var_, pol)
    case joint: TJointType =>
      inlineJoint(joint, var_, pol)
    case app: TApp =>
      inlineApp(app, var_, pol)
    case TUniv(typeVar, _) if typeVar == var_ =>
      type_
    case univ: TUniv =>
      inlineUniv(univ, var_, pol)
    case constrained: TConstrained =>
      inlineConstrained(constrained, var_, pol)
    case constraining: TConstraining =>
      inlineConstraining(constraining, var_, pol)
    case TBot | TTop | TVar(_) | TClass(_) =>
      type_

private def inlineNegation(neg: TNeg, var_ : TypeVar, pol: Polarity)(using ctx: Context): Type =
  simplifyNegation(
    inlineType(neg.body, var_, !pol)
  )

private def inlineTuple(tuple: TTuple, var_ : TypeVar, pol: Polarity)(using ctx: Context): Type =
  val newLeft = inlineType(tuple.left, var_, pol)
  val newRight = inlineType(tuple.right, var_, pol)
  if newLeft == tuple.left && newRight == tuple.right then
    tuple
  else
    TTuple(newLeft, newRight)

private def inlineLambda(lam: TLam, var_ : TypeVar, pol: Polarity)(using ctx: Context): Type =
  simplifyLambda(
    inlineType(lam.param, var_, !pol),
    inlineType(lam.ret, var_, pol),
  )

private def inlineJoint(joint: TJointType, var_ : TypeVar, pol: Polarity)(using ctx: Context): Type =
  val newLeft = inlineType(joint.left, var_, pol)
  val newRight = inlineType(joint.right, var_, pol)
  if newLeft == joint.left && newRight == joint.right then
    joint
  else
    simplifyJoint(joint.mode, newLeft, newRight)

private def inlineApp(app: TApp, var_ : TypeVar, pol: Polarity)(using ctx: Context): Type =
  val newAbs = inlineType(app.abs, var_, pol)
  val newArg = inlineType(app.arg, var_, pol)
  if newAbs == app.abs && newArg == app.arg then
    app
  else
    TApp(newAbs, newArg)

private def inlineUniv(univ: TUniv, var_ : TypeVar, pol: Polarity)(using ctx: Context): Type =
  val newBody = inlineType(univ.body, var_, pol)(using ctx.declTypeVar(univ.var_, TypeVarKind.Rigid))
  simplifyUniv(univ.var_, newBody)

private def inlineConstrained(constrained: TConstrained, var_ : TypeVar, pol: Polarity)(using ctx: Context): Type =
  val newBody = inlineType(constrained.body, var_, pol)
  val newConstraint = inlineConstraint(constrained.constraint, var_)
  if newBody == constrained.body && newConstraint == constrained.constraint then
    constrained
  else
    simplifyConstrained(newBody, newConstraint)

private def inlineConstraining(constraining: TConstraining, var_ : TypeVar, pol: Polarity)(using ctx: Context): Type =
  val newBody = inlineType(constraining.body, var_, pol)
  val newConstraint = inlineConstraint(constraining.constraint, var_)
  if newBody == constraining.body && newConstraint == constraining.constraint then
    constraining
  else
    simplifyConstraining(newBody, newConstraint)

/** Inline a variable in a constraint using the current direction-based polarity convention. */
private def inlineConstraint(constraint: Constraint, var_ : TypeVar)(using ctx: Context): Constraint =
  Constraint(
    inlineType(constraint.left, var_, constraint.dir.rightPol),
    constraint.dir,
    inlineType(constraint.right, var_, constraint.dir.leftPol),
  )
