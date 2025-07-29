package hkmc2.ctml.core.abstractions

import hkmc2.ctml.types.*
import hkmc2.ctml.core.debug.*

abstract class TypeDispatcher[F[+_], P](combinator: TypeCombinator[F]) extends TypeApplicator[F, P], BoundsApplicator[F, P]:
  override def apply(type_ : Type, p: P): F[Type] =
    type_ match
      case TBot =>
        combinator.bot()
      case TTop =>
        combinator.top()
      case TVar(var_) =>
        combinator.var_(var_)
      case TLam(param, ret) =>
        combinator.lam(
          this.apply(param, p),
          this.apply(ret, p),
        )
      case TUnion(left, right) =>
        combinator.union(
          this.apply(left, p),
          this.apply(right, p),
        )
      case TInter(left, right) =>
        combinator.inter(
          this.apply(left, p),
          this.apply(right, p),
        )
      case TUniv(var_, body) =>
        combinator.univ(
          var_,
          this.apply(body, p)
        )
      case TConstrained(body, bounds) =>
        combinator.constrained(
          this.apply(body, p),
          this.apply(bounds, p),
        )
      case TConstraining(body, bounds) =>
        combinator.constraining(
          this.apply(body, p),
          this.apply(bounds, p),
        )
