package hkmc2.ctml.core.type_.impls.simplify

import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.type_.impls.getAllVarPolarities.getAllVarPolarities
import scala.collection.immutable.LazyList.cons
import hkmc2.ctml.core.context.extend
import hkmc2.ctml.core.structural.isIndirectRecursive
import hkmc2.ctml.core.subtyping.checkEqual
import hkmc2.ctml.core.context.lowerBound
import hkmc2.ctml.core.context.upperBound
import hkmc2.ctml.core.inference.inlineVar

/** Implementation of semantic type simplification. */
extension (type_ : Type)
  /** Simplify the type based on the information available in a context. */
  def simplify()(using ctx: Context, noInlineVars: NoInlineVars): Type =
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
        val newBody = body.simplify()(using ctx.declTypeVar(var_, TypeVarKind.Rigid), noInlineVars)
        simplifyUniv(var_, newBody)
      case TConstrained(body, constraint) =>
        simplifyConstrained(
          body.simplify(),
          constraint.simplify(),
        )
      case TConstraining(body, constraint) =>
        simplifyConstraining(
          body.simplify(),
          constraint.simplify(),
        )

extension (univ: TUniv)
  def simplify()(using ctx: Context, noInlineVars: NoInlineVars): Type =
    val (body, constraints) = univ.getConstrainedComponents

    val bounds = constraints.map(_.asBound)

    body.getInlinePolarities(univ.var_)(using ctx.extend(bounds), noInlineVars) match
      case Some(polarities) =>
        val (newBody, newClauses) = inlineVar(body, univ.var_, polarities, Clauses(bounds))
        newBody.wrapCtx(newClauses)
      case None =>
        univ

extension (constraint: Constraint)
  def simplify()(using ctx: Context, noInlineVars: NoInlineVars): Constraint =
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
