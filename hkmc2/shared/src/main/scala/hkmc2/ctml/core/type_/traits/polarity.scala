package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*

/** Trait for objects that carry a polarity. */
trait WithPolarity[This <: WithPolarity[This]]:
  /** Get the polarity of the object. */
  def getPolarity: Polarity
  /** Set the polarity of the object. */
  def setPolarity(pol: Polarity): This

/** Applicator that recursively applies a combinator on the components of a type while tracking the
 *  type polarity. */
abstract class TypePolarityApplicator[T[+_], B[+_], P <: WithPolarity[P]](
  next: TypeApplicator[T, P] & TypeNode[T, B, P]
) extends TypeApplicator[T, P], BoundApplicator[B, P], TypeNode[T, B, P]:
  override def getCombinator: TypeCombinator[T, B, P] = next.getCombinator

  override def apply(type_ : Type, params: P)(using first: TypeApplicator[T, P]): T[Type] =
    type_ match
      case TLam(param, ret) =>
        val pol = params.getPolarity
        next.getCombinator.lam(
          first.apply(param, params.setPolarity(pol.invert())),
          first.apply(ret, params),
          params,
        )
      case _ =>
        next.apply(type_, params)

  override def apply(bound: Bound, params: P): B[Bound] =
    val pol = bound.dir match
      case Direction.Sub =>
        params.getPolarity.invert()
      case Direction.Super =>
        params.getPolarity
    this.bound1(bound, params.setPolarity(pol))

  def bound1(bound: Bound, params: P): B[Bound]
