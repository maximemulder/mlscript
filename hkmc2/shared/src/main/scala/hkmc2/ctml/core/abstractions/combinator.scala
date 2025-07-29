package hkmc2.ctml.core.abstractions

import hkmc2.ctml.types.*

trait TypeCombinator[F[_]]:
  def bot(): F[TBot]

  def top(): F[TTop]

  def var_(var_ : TypeVar): F[TVar]

  def lam(param: F[Type], ret: F[Type]): F[TLam]

  def union(left: F[Type], right: F[Type]): F[TUnion]

  def inter(left: F[Type], right: F[Type]): F[TInter]

  def univ(var_ : TypeVar, body: F[Type]): F[TUniv]

  def constrained(body: F[Type], bounds: F[List[Bound]]): F[TConstrained]

  def constraining(body: F[Type], bounds: F[List[Bound]]): F[TConstraining]

  def bounds(bounds: List[F[Bound]]): F[List[Bound]]

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

  def bounds(bounds: List[Bound]): List[Bound] =
    bounds
