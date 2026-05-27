package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*

/** Combinator to combine the components of a type into a monoidal value. */
final class TypeMonoidCombinator[T, P](m: Monoid[T]) extends TypeCombinator[Const[T], Const[T], P], ConstraintCombinator[Const[T], Const[T], P]:
  def bot(p: P): T =
    m.empty

  def top(p: P): T =
    m.empty

  def neg(body: T, p: P): T =
    body

  def var_(var_ : TypeVar): T =
    m.empty

  def class_(var_ : ClassVar): T =
    m.empty

  def tuple(left: T, right: T, p: P): T =
    m.combine(left, right)

  def lam(param: T, ret: T, p: P): T =
    m.combine(param, ret)

  def union(left: T, right: T, p: P): T =
    m.combine(left, right)

  def inter(left: T, right: T, p: P): T =
    m.combine(left, right)

  def app(abs: T, arg: T, p: P): T =
    m.combine(abs, arg)

  def univ(var_ : TypeVar, body: T, p: P): T =
    body

  def constrained(body: T, bounds: T, p: P): T =
    m.combine(body, bounds)

  def constraining(body: T, bounds: T, p: P): T =
    m.combine(body, bounds)

  def constraint(left: T, dir: Direction, right: T, p: P): T =
    m.combine(left, right)
