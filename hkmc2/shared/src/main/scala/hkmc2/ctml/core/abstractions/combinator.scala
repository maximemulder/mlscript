package hkmc2.ctml.core.abstractions

import hkmc2.ctml.util.*
import hkmc2.ctml.types.*

trait TypeCombinator[T[_], B[_], P]:
  def bot(p: P): T[TBot]

  def top(p: P): T[TTop]

  def var_(var_ : TypeVar): T[TVar]

  def lam(param: T[Type], ret: T[Type], p: P): T[TLam]

  def union(left: T[Type], right: T[Type], p: P): T[TUnion]

  def inter(left: T[Type], right: T[Type], p: P): T[TInter]

  def univ(var_ : TypeVar, body: T[Type], p: P): T[TUniv]

  def constrained(body: T[Type], bounds: B[List[Bound]], p: P): T[TConstrained]

  def constraining(body: T[Type], bounds: B[List[Bound]], p: P): T[TConstraining]

  def bounds(bounds: List[B[Bound]], p: P): B[List[Bound]]

class TypeIdentityCombinator[P] extends TypeCombinator[Id, Id, P]:
  def bot(p: P): TBot =
    TBot

  def top(p: P): TTop =
    TTop

  def var_(var_ : TypeVar): TVar =
    TVar(var_)

  def lam(param: Type, ret: Type, p: P): TLam =
    TLam(param, ret)

  def union(left: Type, right: Type, p: P): TUnion =
    TUnion(left, right)

  def inter(left: Type, right: Type, p: P): TInter =
    TInter(left, right)

  def univ(var_ : TypeVar, body: Type, p: P): TUniv =
    TUniv(var_, body)

  def constrained(body: Type, bounds: List[Bound], p: P): TConstrained =
    TConstrained(body, bounds)

  def constraining(body: Type, bounds: List[Bound], p: P): TConstraining =
    TConstraining(body, bounds)

  def bounds(bounds: List[Bound], p: P): List[Bound] =
    bounds
