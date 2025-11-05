package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.util.*
import hkmc2.ctml.types.*

/** Trait that describes the combination of some possibly transformed type components. */
trait TypeCombinator[T[_], B[_], P]:
  /** Get a bottom type combination. */
  def bot(p: P): T[TBot]

  /** Get a top type combination. */
  def top(p: P): T[TTop]

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
  def constrained(body: T[Type], bounds: B[Bound], p: P): T[TConstrained]

  /** Get a constraining type combination. */
  def constraining(body: T[Type], bounds: B[List[Bound]], p: P): T[TConstraining]

  /** Get some type variable bounds combination. */
  def bounds(bounds: List[B[Bound]], p: P): B[List[Bound]]

/** Trait that describes a node in an operation that can forward the final combinator of the
 *  operation. */
trait TypeNode[T[_], B[_], P]:
  /** Get the final combinator of the operation. */
  def getCombinator: TypeCombinator[T, B, P]
