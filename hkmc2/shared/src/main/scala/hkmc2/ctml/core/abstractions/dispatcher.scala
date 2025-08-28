package hkmc2.ctml.core.abstractions

import hkmc2.ctml.types.*
import hkmc2.ctml.core.debug.*

/** Applicator that recursively applies a combinator on the components of a type. */
abstract class TypeDispatcher[T[+_], B[+_], P](combinator: TypeCombinator[T, B, P]) extends TypeApplicator[T, P], BoundsApplicator[B, P], TypeNode[T, B, P]:
  override def getCombinator = this.combinator

  override def apply(type_ : Type, p: P): T[Type] =
    type_ match
      case TBot =>
        combinator.bot(p)
      case TTop =>
        combinator.top(p)
      case TVar(var_) =>
        combinator.var_(var_)
      case TTuple(left, right) =>
        combinator.tuple(
          this.apply(left, p),
          this.apply(right, p),
          p,
        )
      case TLam(param, ret) =>
        combinator.lam(
          this.apply(param, p),
          this.apply(ret, p),
          p,
        )
      case TUnion(left, right) =>
        combinator.union(
          this.apply(left, p),
          this.apply(right, p),
          p,
        )
      case TInter(left, right) =>
        combinator.inter(
          this.apply(left, p),
          this.apply(right, p),
          p,
        )
      case TApp(abs, arg) =>
        combinator.app(
          this.apply(abs, p),
          this.apply(arg, p),
          p,
        )
      case TUniv(var_, body) =>
        combinator.univ(
          var_,
          this.apply(body, p),
          p,
        )
      case TConstrained(body, bounds) =>
        combinator.constrained(
          this.apply(body, p),
          this.apply(bounds, p),
          p,
        )
      case TConstraining(body, bounds) =>
        combinator.constraining(
          this.apply(body, p),
          this.apply(bounds, p),
          p,
        )
