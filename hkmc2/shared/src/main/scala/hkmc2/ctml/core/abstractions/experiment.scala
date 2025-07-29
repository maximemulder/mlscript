package hkmc2.ctml.core.abstractions

import hkmc2.ctml.util.*
import hkmc2.ctml.types.*

/** Represent an object that has a polarity. */
trait WithPolarity[This <: WithPolarity[This]]:
  /** Get the polarity of the object. */
  def getPolarity: Polarity
  /** Set the polarity of the object. */
  def setPolarity(pol: Polarity): This

abstract class TypePolarityDispatcher[F[+_], P <: WithPolarity[P]](combinator: TypeCombinator[F]) extends TypeDispatcher[F, P](combinator):
  override def apply(type_ : Type, params: P): F[Type] =
    type_ match
      case TLam(param, ret) =>
        val pol = params.getPolarity
        combinator.lam(
          this.apply(param, params.setPolarity(pol.invert())),
          this.apply(ret, params),
        )
      case TConstrained(body, bounds) =>
        combinator.constrained(
          this.apply(body, params),
          this.apply(bounds, params),
        )
      case TConstraining(body, bounds) =>
        combinator.constraining(
          this.apply(body, params),
          this.apply(bounds, params),
        )
      case _ =>
        super.apply(type_, params)

  override def apply(bounds: List[Bound], params: P): F[List[Bound]] =
    combinator.bounds(bounds.map(bound =>
      val pol = bound.dir match
        case Direction.Sub =>
          params.getPolarity.invert()
        case Direction.Super =>
          params.getPolarity
      this.bound(bound, params.setPolarity(pol))
    ))

  def bound(bound: Bound, params: P): F[Bound]

class TypeMonoidCombinator[T](m: Monoid[T]) extends TypeCombinator[[_] =>> T]:
  def bot(): T =
    m.empty

  def top(): T =
    m.empty

  def var_(var_ : TypeVar): T =
    m.empty

  def lam(param: T, ret: T): T =
    m.combine(param, ret)

  def union(left: T, right: T): T =
    m.combine(left, right)

  def inter(left: T, right: T): T =
    m.combine(left, right)

  def univ(var_ : TypeVar, body: T): T =
    body

  def constrained(body: T, bounds: T): T =
    m.combine(body, bounds)

  def constraining(body: T, bounds: T): T =
    m.combine(body, bounds)

  def bounds(bounds: List[T]): T =
    m.combineMany(bounds)
