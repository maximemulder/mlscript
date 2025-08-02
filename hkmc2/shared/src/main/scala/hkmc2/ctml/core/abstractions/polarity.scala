package hkmc2.ctml.core.abstractions

import hkmc2.ctml.types.*

/** Trait for objects that carry a polarity. */
trait WithPolarity[This <: WithPolarity[This]]:
  /** Get the polarity of the object. */
  def getPolarity: Polarity
  /** Set the polarity of the object. */
  def setPolarity(pol: Polarity): This

/** Applicator that recursively applies a combinator on the components of a type while tracking the
 *  type polarity. */
abstract class TypePolarityDispatcher[T[+_], B[+_], P <: WithPolarity[P]](
  combinator: TypeCombinator[T, B, P]
) extends TypeDispatcher[T, B, P](combinator):
  override def apply(type_ : Type, params: P): T[Type] =
    type_ match
      case TLam(param, ret) =>
        val pol = params.getPolarity
        combinator.lam(
          this.apply(param, params.setPolarity(pol.invert())),
          this.apply(ret, params),
          params,
        )
      case _ =>
        super.apply(type_, params)

  override def apply(bounds: List[Bound], params: P): B[List[Bound]] =
    combinator.bounds(bounds.map(bound =>
      val pol = bound.dir match
        case Direction.Sub =>
          params.getPolarity.invert()
        case Direction.Super =>
          params.getPolarity
      this.bound(bound, params.setPolarity(pol))
    ), params)

  def bound(bound: Bound, params: P): B[Bound]
