package hkmc2.ctml.core.simplification

import hkmc2.ctml.core.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Replace a variable by its effective lower bound at positive occurrences and by its
   *  effective upper bound at negative occurrences, simplifying the result. */
  private[simplification] def inline(var_ : TypeVar)(using ctx: SubContext): Type =
    inlineType(type_, var_, Polarity.Positive)

extension (bound: Bound)
  /** Inline a variable in a bound, excluding direct references to the bounded variable from
   *  its replacement so that the relation between the two variables does not become recursive. */
  private[simplification] def inline(var_ : TypeVar)(using ctx: SubContext): Bound =
    val newUpper = var_.upperBound.removeDirectVar(bound.var_, Polarity.Negative)
    val newLower = var_.lowerBound.removeDirectVar(bound.var_, Polarity.Positive)
    // The bounds of the inlined variable without the bounded variable override its bounds.
    val newBoundType = inlineType(bound.type_, var_, bound.dir.leftPol)(using
      ctx.overrideVarBounds(var_, newUpper, newLower)
    )
      .removeDirectVar(bound.var_, bound.dir.leftPol)

    bound.copy(type_ = newBoundType)

/** Implementation of semantic type variable inlining. */
private def inlineType(type_ : Type, var_ : TypeVar, pol: Polarity)(using ctx: SubContext): Type =
  type_ match
    case TVar(typeVar) if typeVar == var_ =>
      var_.bound(pol.dir).removeDirectVar(var_, pol)
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
    case TBot | TTop | TVar(_) | TClass(_) =>
      type_

/** Inline under a negation, reversing the replacement polarity. */
private def inlineNegation(neg: TNeg, var_ : TypeVar, pol: Polarity)(using ctx: SubContext): Type =
  makeNegationType(
    inlineType(neg.body, var_, !pol)
  )

/** Inline in both covariant tuple components. */
private def inlineTuple(tuple: TTuple, var_ : TypeVar, pol: Polarity)(using ctx: SubContext): Type =
  val newLeft = inlineType(tuple.left, var_, pol)
  val newRight = inlineType(tuple.right, var_, pol)
  if newLeft == tuple.left && newRight == tuple.right then
    tuple
  else
    TTuple(newLeft, newRight)

/** Inline in a function, reversing the polarity of its parameter. */
private def inlineLambda(lam: TLam, var_ : TypeVar, pol: Polarity)(using ctx: SubContext): Type =
  makeLambdaType(
    inlineType(lam.param, var_, !pol),
    inlineType(lam.ret, var_, pol),
  )

/** Inline in both operands, combining them only when an operand changes. */
private def inlineJoint(joint: TJointType, var_ : TypeVar, pol: Polarity)(using ctx: SubContext): Type =
  val newLeft = inlineType(joint.left, var_, pol)
  val newRight = inlineType(joint.right, var_, pol)
  if newLeft == joint.left && newRight == joint.right then
    joint
  else
    simplifyJoint(joint.mode, newLeft, newRight)

/** Inline in both components of a type application. */
private def inlineApp(app: TApp, var_ : TypeVar, pol: Polarity)(using ctx: SubContext): Type =
  val newAbs = inlineType(app.abs, var_, pol)
  val newArg = inlineType(app.arg, var_, pol)
  if newAbs == app.abs && newArg == app.arg then
    app
  else
    TApp(newAbs, newArg)

/** Inline under a universal binder with its variable declared in the context. */
private def inlineUniv(univ: TUniv, var_ : TypeVar, pol: Polarity)(using ctx: SubContext): Type =
  val newBody = inlineType(univ.body, var_, pol)(using ctx.declTypeVar(univ.var_, TypeVarKind.Rigid))
  simplifyUniv(univ.var_, newBody)

/** Inline in a constrained type and its guard. */
private def inlineConstrained(constrained: TConstrained, var_ : TypeVar, pol: Polarity)(using ctx: SubContext): Type =
  val newBody = inlineType(constrained.body, var_, pol)
  val newConstraint = inlineConstraint(constrained.constraint, var_)
  if newBody == constrained.body && newConstraint == constrained.constraint then
    constrained
  else
    simplifyConstrained(newBody, newConstraint)

/** Inline a variable in a constraint using the current direction-based polarity convention. */
private def inlineConstraint(constraint: Constraint, var_ : TypeVar)(using ctx: SubContext): Constraint =
  Constraint(
    inlineType(constraint.left, var_, constraint.dir.rightPol),
    constraint.dir,
    inlineType(constraint.right, var_, constraint.dir.leftPol),
  )
