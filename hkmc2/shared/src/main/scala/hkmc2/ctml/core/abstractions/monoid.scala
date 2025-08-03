package hkmc2.ctml.core.abstractions

import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

/** Combinator to combine the components of a type into a monoidal value. */
class TypeMonoidCombinator[T, P](m: Monoid[T]) extends TypeCombinator[Const[T], Const[T], P]:
  def bot(p: P): T =
    m.empty

  def top(p: P): T =
    m.empty

  def var_(var_ : TypeVar): T =
    m.empty

  def tuple(left: T, right: T, p: P): T =
    m.empty

  def lam(param: T, ret: T, p: P): T =
    m.combine(param, ret)

  def union(left: T, right: T, p: P): T =
    m.combine(left, right)

  def inter(left: T, right: T, p: P): T =
    m.combine(left, right)

  def univ(var_ : TypeVar, body: T, p: P): T =
    body

  def constrained(body: T, bounds: T, p: P): T =
    m.combine(body, bounds)

  def constraining(body: T, bounds: T, p: P): T =
    m.combine(body, bounds)

  def bounds(bounds: List[T], p: P): T =
    m.combineMany(bounds)
