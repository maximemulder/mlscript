package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*

/** Applicator that recursively applies a combinator on the components of a type while tracking the
 *  type polarity. */
final class TypePolarityApplicator[T[+_], B[+_], P <: PolarityParams[P]](
  next: TypeApplicator[T, P] & ConstraintApplicator[T, B, P],
  last: TypeCombinator[T, B, P] & ConstraintCombinator[T, B, P],
) extends TypeApplicator[T, P], ConstraintApplicator[T, B, P]:
  override def apply(type_ : Type, params: P)(using first: TypeApplicator[T, P]): T[Type] =
    type_ match
      case TLam(param, ret) =>
        val pol = params.pol
        last.lam(
          first.apply(param, params.setPolarity(pol.invert)),
          first.apply(ret, params),
          params,
        )
      case _ =>
        next.apply(type_, params)

  override def apply(constraint: Constraint, params: P)(using first: TypeApplicator[T, P]): B[Constraint] =
    last.constraint(
      first.apply(constraint.left, params.setPolarity(constraint.dir.pol.product(params.pol))),
      constraint.dir,
      first.apply(constraint.right, params.setPolarity(constraint.dir.pol.product(params.pol.invert))),
      params,
    )
