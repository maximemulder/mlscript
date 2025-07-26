package hkmc2.ctml.core.type_

import hkmc2.ctml.util.*
import hkmc2.ctml.types.*

// There are three kinds of combinations that are interesting:
// - identity (forall a. a -> a)
// - constant (forall a. a -> b)
// - identity + generic metadata

abstract class TypeApplicator[F[+_], P]:
  def apply(type_ : Type, p: P): F[Type]

class TypePolarity[F[+_]](applicator: TypeApplicator[F, Polarity], combinator: TypeCombinator[F]) extends TypeApplicator[F, Polarity]:
  def apply(type_ : Type, p: Polarity): F[Type] =
    type_ match
      case TLam(param, ret) =>
        combinator.lam(
          applicator.apply(param, p.invert()),
          applicator.apply(ret, p),
        )
      case _ =>
        applicator.apply(type_, p)

class TypeCombinatorApplicator[F[+_], P](combinator: TypeCombinator[F]) extends TypeApplicator[F, P]:
  def apply(type_ : Type, p: P): F[Type] =
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
          bounds,
        )
      case TConstraining(body, bounds) =>
        combinator.constraining(
          this.apply(body, p),
          bounds,
        )

abstract class TypeCombinator[F[_]]:
  def bot(): F[TBot]

  def top(): F[TTop]

  def var_(var_ : TypeVar): F[TVar]

  def lam(param: F[Type], ret: F[Type]): F[TLam]

  def union(left: F[Type], right: F[Type]): F[TUnion]

  def inter(left: F[Type], right: F[Type]): F[TInter]

  def univ(var_ : TypeVar, body: F[Type]): F[TUniv]

  def constrained(body: F[Type], bounds: List[Bound]): F[TConstrained]

  def constraining(body: F[Type], bounds: List[Bound]): F[TConstraining]

object TypeIdentityCombinator extends TypeCombinator[[T] =>> T]:
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

class TypeMonoidCombinator[T](using m: Monoid[T]) extends TypeCombinator[[_] =>> T]:
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

def TypeIdentity                              = TypeCombinatorApplicator[[T] =>> T, Unit](TypeIdentityCombinator)

def TypeMonoidUnit[T](using m: Monoid[T])     = TypeCombinatorApplicator[[_] =>> T, Unit](new TypeMonoidCombinator)

def TypeMonoid[T](using m: Monoid[T])         = TypeCombinatorApplicator[[_] =>> T, Polarity](new TypeMonoidCombinator)

def TypeMonoidPolarity[T](using m: Monoid[T]) = TypePolarity(TypeMonoid, new TypeMonoidCombinator)
