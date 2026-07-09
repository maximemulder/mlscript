package hkmc2.ctml.core.type_

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.makeConstrainedType
import hkmc2.ctml.core.clauses.*
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
  def unwrapCtx(using ctx: SubContext): (Type, SubClauses) =
    val (vars, univBody) = type_.getUnivComponents
    val (constrainedBody, constraints) = univBody.getConstrainedComponents
    val univOuts = SubClauses(vars.reverse.map(TypeVarDecl(_, TypeVarKind.Flex, None, ctx.level)))
    val constrainedOuts = constraints.foldLeft(univOuts)((outs, constraint) =>
      subtypeConstraintSeq(constraint, outs)(using ctx, ConstraintMode.Solve)
    )

    (constrainedBody, constrainedOuts)

  /** Wrap contextual information around a type using universal and constrained types. */
  def wrapCtx(clauses: SubClauses): Type =
    val constrained = makeConstrainedType(type_, clauses.bounds.map(_.toConstraint))

    clauses.typeVarDecls.foldLeft(constrained)((body, decl) =>
      TUniv(decl.var_, body)
    )

  /** Quantify/wrap contextual information around a type. */
  def quantifyCtx(clauses: SubClauses): Type =
    type_.wrapCtx(clauses)

private def hoistTypeCtx(type_ : Type, parent: (Type) => Type): Type =
  type_ match
    case TUniv(var_, body) =>
      TUniv(var_, hoistTypeCtx(body, parent))
    case TConstrained(body, constraint) =>
      TConstrained(hoistTypeCtx(body, parent), constraint)
    case TLam(param, ret) =>
      hoistTypeCtx(ret, (ret) => parent(TLam(param, ret)))
    case TJointType(mode, left, right) =>
      hoistTypeCtx(left, (left) =>
        hoistTypeCtx(right, (right) =>
          parent(TJointType(mode, left, right))
        )
      )
    case _ =>
      parent(type_)
