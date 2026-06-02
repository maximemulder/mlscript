package hkmc2.ctml.core.type_

import hkmc2.ctml.types.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.subtyping.*

extension (type_ : Type)
  /** Hoist the contextual information (quantified variables and constraints) in a type to the top
   *  level. */
  def hoistCtx: Type =
    type_ match
      case TLam(param, TUniv(var_, body)) =>
        TUniv(var_, TLam(param, body).hoistCtx)
      case TLam(param, TConstrained(body, constraint)) =>
        TConstrained(TLam(param, body).hoistCtx, constraint)
      case TLam(param, TConstraining(body, constraint)) =>
        TConstraining(TLam(param, body).hoistCtx, constraint)
      case _ =>
        type_

  /** Unwrap the contextual information (quantified variables and constraints) in the top level of
   *  a type. */
  def unwrapCtx(using ctx: Context): (Type, Clauses) =
    val (vars, univBody) = type_.getUnivComponents
    val (constrainedBody, constraints) = univBody.getConstrainedComponents
    val univOuts = Clauses(vars.map(TypeVarDecl(_, TypeVarKind.Flex, None, ctx.maxLevel)))
    val constrainedOuts = constraints.foldRight(univOuts)((constraint, outs) =>
      subtypeConstraintSeq(constraint, outs)(using ctx, ConstraintMode.Solve)
    )

    (constrainedBody, constrainedOuts)
