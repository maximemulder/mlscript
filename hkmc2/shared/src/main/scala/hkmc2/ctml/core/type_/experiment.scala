package hkmc2.ctml.core.type_

import hkmc2.ctml.util.*
import hkmc2.ctml.types.*

// There are three kinds of combinations that are interesting:
// - identity (forall a. a -> a)
// - constant (forall a. a -> b)
// - identity + generic metadata

abstract class TypeApply[F[+_], P]:
  def type_(type_ : Type, p: P): F[Type]

class TypeApplyPolarity[F[+_]](apply: TypeApply[F, Polarity], combinator: TypeCombine[F]) extends TypeApply[F, Polarity]:
  def type_(type_ : Type, p: Polarity): F[Type] =
    type_ match
      case TLam(param, ret) =>
        combinator.lam(
          apply.type_(param, p.invert()),
          apply.type_(ret, p),
        )
      case _ =>
        apply.type_(type_, p)

class TypeApplyCombine[F[+_], P](combinator: TypeCombine[F]) extends TypeApply[F, P]:
  def type_(type_ : Type, p: P): F[Type] =
    type_ match
      case TBot =>
        combinator.bot()
      case TTop =>
        combinator.top()
      case TVar(var_) =>
        combinator.var_(var_)
      case TLam(param, ret) =>
        combinator.lam(
          this.type_(param, p),
          this.type_(ret, p),
        )
      case TUnion(left, right) =>
        combinator.union(
          this.type_(left, p),
          this.type_(right, p),
        )
      case TInter(left, right) =>
        combinator.inter(
          this.type_(left, p),
          this.type_(right, p),
        )
      case TUniv(var_, body) =>
        combinator.univ(
          var_,
          this.type_(body, p)
        )
      case TConstrained(body, bounds) =>
        combinator.constrained(
          this.type_(body, p),
          bounds,
        )
      case TConstraining(body, bounds) =>
        combinator.constraining(
          this.type_(body, p),
          bounds,
        )

abstract class TypeCombine[F[_]]:
  def bot(): F[TBot]

  def top(): F[TTop]

  def var_(var_ : TypeVar): F[TVar]

  def lam(param: F[Type], ret: F[Type]): F[TLam]

  def union(left: F[Type], right: F[Type]): F[TUnion]

  def inter(left: F[Type], right: F[Type]): F[TInter]

  def univ(var_ : TypeVar, body: F[Type]): F[TUniv]

  def constrained(body: F[Type], bounds: List[Bound]): F[TConstrained]

  def constraining(body: F[Type], bounds: List[Bound]): F[TConstraining]

object TypeCombineIdentiy extends TypeCombine[[T] =>> T]:
  def bot(): TBot =
    TBot

  def top(): TTop =
    TTop

  def var_(var_ : TypeVar): TVar =
    TVar(var_)

  def lam(param: Type, ret: Type): TLam =
    TLam(param, ret)

  def union(left: Type, right: Type): TUnion =
    TUnion(left, right)

  def inter(left: Type, right: Type): TInter =
    TInter(left, right)

  def univ(var_ : TypeVar, body: Type): TUniv =
    TUniv(var_, body)

  def constrained(body: Type, bounds: List[Bound]): TConstrained =
    TConstrained(body, bounds)

  def constraining(body: Type, bounds: List[Bound]): TConstraining =
    TConstraining(body, bounds)

class TypeCombineMonoid[T](using m: Monoid[T]) extends TypeCombine[[_] =>> T]:
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

  def constrained(body: T, bounds: List[Bound]): T =
    body

  def constraining(body: T, bounds: List[Bound]): T =
    body

def TypeApplyIdentity                      = TypeApplyCombine[[T] =>> T, Unit](TypeCombineIdentiy)

def TypeApplyMonoid[T](using m: Monoid[T]) = TypeApplyCombine[[_] =>> T, Unit](new TypeCombineMonoid)

def TypeApplyMonoidB[T](using m: Monoid[T]) = TypeApplyCombine[[_] =>> T, Polarity](new TypeCombineMonoid)

def TypeApplyPolarityMonoid[T](using m: Monoid[T]) = TypeApplyPolarity(TypeApplyMonoidB, new TypeCombineMonoid)
