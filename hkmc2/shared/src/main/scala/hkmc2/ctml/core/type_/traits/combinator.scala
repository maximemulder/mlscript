package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*

/** Trait that describes the combination of some possibly transformed type components. */
trait TypeCombinator[T[_], B[_], P]:
  /** Get a bottom type combination. */
  def bot(p: P): T[TBot]

  /** Get a top type combination. */
  def top(p: P): T[TTop]

  /** Get a negation type combination. */
  def neg(body: T[Type], p: P): T[TNeg]

  /** Get a type variable type combination. */
  def var_(var_ : TypeVar): T[TVar]

  /** Get a tuple type combination. */
  def tuple(left: T[Type], right: T[Type], p: P): T[TTuple]

  /** Get a lambda type combination. */
  def lam(param: T[Type], ret: T[Type], p: P): T[TLam]

  /** Get a union type combination. */
  def union(left: T[Type], right: T[Type], p: P): T[TUnion]

  /** Get an intersection type combination. */
  def inter(left: T[Type], right: T[Type], p: P): T[TInter]

  /** Get a type application combination. */
  def app(abs: T[Type], arg: T[Type], p: P): T[TApp]

  /** Get a universal type combination. */
  def univ(var_ : TypeVar, body: T[Type], p: P): T[TUniv]

  /** Get a constrained type combination. */
  def constrained(body: T[Type], constraint: B[Constraint], p: P): T[TConstrained]

  /** Get a constraining type combination. */
  def constraining(body: T[Type], constraint: B[Constraint], p: P): T[TConstraining]

/** Trait that describes the combination of some possibly transformed constraint components. */
trait ConstraintCombinator[T[_], B[_], P]:
  /** Get a constraint combination. */
  def constraint(left: T[Type], dir: Direction, right: T[Type], p: P): B[Constraint]
