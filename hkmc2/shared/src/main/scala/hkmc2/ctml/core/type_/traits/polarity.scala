package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*

/** Applicator that recursively applies a combinator on the components of a type while tracking the
 *  type polarity. */
final class TypePolarityApplicator[T[+_], B[+_], P <: PolarityParams[P]](
  next: TypeApplicator[T, B, P],
  last: TypeCombinator[T, B, P] & ConstraintCombinator[T, B, P],
) extends TypeChainApplicator[T, B, P](next):
  override def apply(type_ : Type, params: P)(using first: TypeApplicator[T, B, P]): T[Type] =
    type_ match
      case TNeg(body) =>
        last.neg(
          first.apply(body, params.setPolarity(params.pol.invert)),
          params,
        )
      case TLam(param, ret) =>
        last.lam(
          first.apply(param, params.setPolarity(params.pol.invert)),
          first.apply(ret, params),
          params,
        )
      // case TConstrained(body, constraint) =>
      //   last.constrained(
      //     first.apply(body, params),
      //     first.apply(constraint, params.setPolarity(params.pol.invert)),
      //     params,
      //   )
      case _ =>
        next.apply(type_, params)

  override def apply(constraint: Constraint, params: P)(using first: TypeApplicator[T, B, P]): B[Constraint] =
    last.constraint(
      first.apply(constraint.left, params.setPolarity(constraint.dir.rightPol)),
      constraint.dir,
      first.apply(constraint.right, params.setPolarity(constraint.dir.leftPol)),
      params,
    )
