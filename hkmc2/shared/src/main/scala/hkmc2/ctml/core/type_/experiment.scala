package hkmc2.ctml.core.type_

import hkmc2.ctml.util.*
import hkmc2.ctml.types.*

// There are three kinds of combinations that are interesting:
// - identity (forall a. a -> a)
// - constant (forall a. a -> b)
// - identity + generic metadata

abstract class TypeApply[F[+_]]:
  def type_(type_ : Type): F[Type] =
    type_ match
      case bot: TBot =>
        this.bot(bot)
      case top: TTop =>
        this.top(top)
      case var_ : TVar =>
        this.var_(var_)
      case lam : TLam =>
        this.lam(lam)
      case union: TUnion =>
        this.union(union)
      case inter: TInter =>
        this.inter(inter)
      case univ: TUniv =>
        this.univ(univ)
      case constrained: TConstrained =>
        this.constrained(constrained)
      case constraining: TConstraining =>
        this.constraining(constraining)

  def bot(bot: TBot): F[TBot]

  def top(top: TTop): F[TTop]

  def var_(var_ : TVar): F[TVar]

  def lam(lam: TLam): F[TLam]

  def union(union: TUnion): F[TUnion]

  def inter(inter: TInter): F[TInter]

  def univ(univ: TUniv): F[TUniv]

  def constrained(constrained: TConstrained): F[TConstrained]

  def constraining(constraining: TConstraining): F[TConstraining]

class TypeApplyCombine[F[+_]](combinator: TypeCombine[F]) extends TypeApply[F]:
  def bot(bot: TBot): F[TBot] =
    combinator.bot()

  def top(top: TTop): F[TTop] =
    combinator.top()

  def var_(var_ : TVar): F[TVar] =
    combinator.var_(var_.var_)

  def lam(lam: TLam): F[TLam] =
    combinator.lam(
      this.type_(lam.param),
      this.type_(lam.ret),
    )

  def union(union: TUnion): F[TUnion] =
    combinator.union(
      this.type_(union.left),
      this.type_(union.right),
    )

  def inter(inter: TInter): F[TInter] =
    combinator.inter(
      this.type_(inter.left),
      this.type_(inter.right),
    )

  def univ(univ: TUniv): F[TUniv] =
    combinator.univ(
      univ.var_,
      this.type_(univ.body)
    )

  def constrained(constrained: TConstrained): F[TConstrained] =
    combinator.constrained(
      this.type_(constrained.body),
      constrained.bounds,
    )

  def constraining(constraining: TConstraining): F[TConstraining] =
    combinator.constraining(
      this.type_(constraining.body),
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

def TypeApplyIdentity                      = TypeApplyCombine(TypeCombineIdentiy)

def TypeApplyMonoid[T](using m: Monoid[T]) = TypeApplyCombine(new TypeCombineMonoid)
