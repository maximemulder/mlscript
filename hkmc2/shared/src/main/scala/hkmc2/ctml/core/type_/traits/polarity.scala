package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*

/** Applicator that recursively applies a combinator on the components of a type while tracking the
 *  type polarity. */
abstract class TypePolarityApplicator[T[+_], B[+_], P <: PolarityParams[P]](
  next: TypeApplicator[T, P],
  last: TypeCombinator[T, B, P],
) extends TypeApplicator[T, P], BoundApplicator[B, P]:
  override def apply(type_ : Type, params: P)(using first: TypeApplicator[T, P]): T[Type] =
    type_ match
      case TLam(param, ret) =>
        val pol = params.pol
        last.lam(
          first.apply(param, params.setPolarity(pol.invert())),
          first.apply(ret, params),
          params,
        )
      case _ =>
        next.apply(type_, params)

  override def apply(bound: Bound, params: P): B[Bound] =
    val pol = bound.dir match
      case Direction.Sub =>
        params.pol.invert()
      case Direction.Super =>
        params.pol
    this.bound1(bound, params.setPolarity(pol))

  def bound1(bound: Bound, params: P): B[Bound]
