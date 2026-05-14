package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.config.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.syntax.Keyword.`override`

/** Applicator that recursively applies a combinator on the components of a type. */
final class TypeDispatcher[T[+_], B[+_], P](
  last: TypeCombinator[T, B, P] & ConstraintCombinator[T, B, P],
) extends TypeApplicator[T, B, P]:
  override def apply(type_ : Type, p: P)(using first: TypeApplicator[T, B, P]): T[Type] =
    type_ match
      case TBot =>
        last.bot(p)
      case TTop =>
        last.top(p)
      case TVar(var_) =>
        last.var_(var_)
      case TClass(var_) =>
        last.class_(var_)
      case TNeg(body) =>
        last.neg(
          first.apply(body, p),
          p,
        )
      case TTuple(left, right) =>
        last.tuple(
          first.apply(left, p),
          first.apply(right, p),
          p,
        )
      case TLam(param, ret) =>
        last.lam(
          first.apply(param, p),
          first.apply(ret, p),
          p,
        )
      case TUnion(left, right) =>
        last.union(
          first.apply(left, p),
          first.apply(right, p),
          p,
        )
      case TInter(left, right) =>
        last.inter(
          first.apply(left, p),
          first.apply(right, p),
          p,
        )
      case TApp(abs, arg) =>
        last.app(
          first.apply(abs, p),
          first.apply(arg, p),
          p,
        )
      case TUniv(var_, body) =>
        last.univ(
          var_,
          first.apply(body, p),
          p,
        )
      case TConstrained(body, constraint) =>
        last.constrained(
          first.apply(body, p),
          first.apply(constraint, p),
          p,
        )
      case TConstraining(body, constraint) =>
        last.constraining(
          first.apply(body, p),
          first.apply(constraint, p),
          p,
        )

  override def apply(constraint: Constraint, p: P)(using first: TypeApplicator[T, B, P]): B[Constraint] =
    last.constraint(
      first.apply(constraint.left, p),
      constraint.dir,
      first.apply(constraint.right, p),
      p,
    )
