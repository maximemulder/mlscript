package hkmc2.ctml.core.abstractions

import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

class TypeMonoidCombinator[T](m: Monoid[T]) extends TypeCombinator[[_] =>> T]:
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

  def constrained(body: T, bounds: T): T =
    m.combine(body, bounds)

  def constraining(body: T, bounds: T): T =
    m.combine(body, bounds)

  def bounds(bounds: List[T]): T =
    m.combineMany(bounds)
