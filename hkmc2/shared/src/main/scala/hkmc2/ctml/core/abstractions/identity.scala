package hkmc2.ctml.core.abstractions

import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

/** Combinator to combine the components of a type into that type. */
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
