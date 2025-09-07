package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.types.*
import hkmc2.ctml.core.debug.*
import hkmc2.ctml.core.type_.traits.*

/** Applicator that recursively applies a combinator on the components of a type. */
abstract class TypeDispatcher[T[+_], B[+_], P](combinator: TypeCombinator[T, B, P]) extends TypeApplicator[T, P], BoundsApplicator[B, P], TypeNode[T, B, P]:
  override def getCombinator = this.combinator

  override def apply(type_ : Type, p: P)(using first: TypeApplicator[T, P]): T[Type] =
    type_ match
      case TBot =>
        combinator.bot(p)
      case TTop =>
        combinator.top(p)
      case TVar(var_) =>
        combinator.var_(var_)
      case TTuple(left, right) =>
        combinator.tuple(
          first.apply(left, p),
          first.apply(right, p),
          p,
        )
      case TLam(param, ret) =>
        combinator.lam(
          first.apply(param, p),
          first.apply(ret, p),
          p,
        )
      case TUnion(left, right) =>
        combinator.union(
          first.apply(left, p),
          first.apply(right, p),
          p,
        )
      case TInter(left, right) =>
        combinator.inter(
          first.apply(left, p),
          first.apply(right, p),
          p,
        )
      case TApp(abs, arg) =>
        combinator.app(
          first.apply(abs, p),
          first.apply(arg, p),
          p,
        )
      case TUniv(var_, body) =>
        combinator.univ(
          var_,
          first.apply(body, p),
          p,
        )
      case TConstrained(body, bounds) =>
        combinator.constrained(
          first.apply(body, p),
          this.apply(bounds, p),
          p,
        )
      case TConstraining(body, bounds) =>
        combinator.constraining(
          first.apply(body, p),
          this.apply(bounds, p),
          p,
        )
