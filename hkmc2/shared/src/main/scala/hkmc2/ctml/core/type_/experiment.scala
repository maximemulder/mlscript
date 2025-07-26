package hkmc2.ctml.core.type_

import hkmc2.ctml.util.*
import hkmc2.ctml.types.*

// There are three kinds of combinations that are interesting:
// - identity (forall a. a -> a)
// - constant (forall a. a -> b)
// - identity + generic metadata

abstract class TypeApply[F[+_], P]:
  def type_(type_ : Type, p: P): F[Type] =
    type_ match
      case bot: TBot =>
        this.bot(bot, p)
      case top: TTop =>
        this.top(top, p)
      case var_ : TVar =>
        this.var_(var_, p)
      case lam : TLam =>
        this.lam(lam, p)
      case union: TUnion =>
        this.union(union, p)
      case inter: TInter =>
        this.inter(inter, p)
      case univ: TUniv =>
        this.univ(univ, p)
      case constrained: TConstrained =>
        this.constrained(constrained, p)
      case constraining: TConstraining =>
        this.constraining(constraining, p)

  def bot(bot: TBot, p: P): F[TBot]

  def top(top: TTop, p: P): F[TTop]

  def var_(var_ : TVar, p: P): F[TVar]

  def lam(lam: TLam, p: P): F[TLam]

  def union(union: TUnion, p: P): F[TUnion]

  def inter(inter: TInter, p: P): F[TInter]

  def univ(univ: TUniv, p: P): F[TUniv]

  def constrained(constrained: TConstrained, p: P): F[TConstrained]

  def constraining(constraining: TConstraining, p: P): F[TConstraining]

class TypeApplyPolarity[F[+_]](apply: TypeApply[F, Polarity], combinator: TypeCombine[F]) extends TypeApply[F, Polarity]:
  def bot(bot: TBot, p: Polarity): F[TBot] =
    apply.bot(bot, p)

  def top(top: TTop, p: Polarity): F[TTop] =
    apply.top(top, p)

  def var_(var_ : TVar, p: Polarity): F[TVar] =
    apply.var_(var_, p)

  def lam(lam: TLam, p: Polarity): F[TLam] =
    combinator.lam(
      apply.type_(lam.param, p.invert()),
      apply.type_(lam.ret, p),
    )

  def union(union: TUnion, p: Polarity): F[TUnion] =
    apply.union(union, p)

  def inter(inter: TInter, p: Polarity): F[TInter] =
    apply.inter(inter, p)

  def univ(univ: TUniv, p: Polarity): F[TUniv] =
    apply.univ(univ, p)

  def constrained(constrained: TConstrained, p: Polarity): F[TConstrained] =
    apply.constrained(constrained, p)

  def constraining(constraining: TConstraining, p: Polarity): F[TConstraining] =
    apply.constraining(constraining, p)

class TypeApplyCombine[F[+_], P](combinator: TypeCombine[F]) extends TypeApply[F, P]:
  def bot(bot: TBot, p: P): F[TBot] =
    combinator.bot()

  def top(top: TTop, p: P): F[TTop] =
    combinator.top()

  def var_(var_ : TVar, p: P): F[TVar] =
    combinator.var_(var_.var_)

  def lam(lam: TLam, p: P): F[TLam] =
    combinator.lam(
      this.type_(lam.param, p),
      this.type_(lam.ret, p),
    )

  def union(union: TUnion, p: P): F[TUnion] =
    combinator.union(
      this.type_(union.left, p),
      this.type_(union.right, p),
    )

  def inter(inter: TInter, p: P): F[TInter] =
    combinator.inter(
      this.type_(inter.left, p),
      this.type_(inter.right, p),
    )

  def univ(univ: TUniv, p: P): F[TUniv] =
    combinator.univ(
      univ.var_,
      this.type_(univ.body, p)
    )

  def constrained(constrained: TConstrained, p: P): F[TConstrained] =
    combinator.constrained(
      this.type_(constrained.body, p),
      constrained.bounds,
    )

  def constraining(constraining: TConstraining, p: P): F[TConstraining] =
    combinator.constraining(
      this.type_(constraining.body, p),
      constraining.bounds,
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
