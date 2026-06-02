package hkmc2.ctml.core.type_

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*

extension (type_ : Type)
  /** Hoist the contextual information (quantified variables and constraints) in a type to the top
   *  level. */
  def hoistCtx: Type =
    hoistTypeCtx(type_, identity)

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

private def hoistTypeCtx(type_ : Type, parent: (Type) => Type): Type =
  type_ match
    case TUniv(var_, body) =>
      TUniv(var_, hoistTypeCtx(body, parent))
    case TConstrained(body, constraint) =>
      TConstrained(hoistTypeCtx(body, parent), constraint)
    case TTuple(left, right) =>
      hoistTypeCtx(left, (left) =>
        hoistTypeCtx(right, (right) =>
          parent(TTuple(left, right))
        )
      )
    case TLam(param, ret) =>
      hoistTypeCtx(ret, (ret) => parent(TLam(param, ret)))
    case TUnion(left, right) =>
      hoistTypeCtx(left, (left) =>
        hoistTypeCtx(right, (right) =>
          parent(TUnion(left, right))
        )
      )
    case TInter(left, right) =>
      hoistTypeCtx(left, (left) =>
        hoistTypeCtx(right, (right) =>
          parent(TInter(left, right))
        )
      )
    case _ =>
      parent(type_)
