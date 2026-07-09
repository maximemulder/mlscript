package hkmc2.ctml.types

import hkmc2.ctml.utils.*
import hkmc2.ctml.core.subtyping.SubtypingCache

/** A subtyping context, which contains the type-level information used by subtyping,
 *  simplification, and level solving.
 */
case class SubContext(
  /** The list of clauses itself. */
  clauses: List[SubClause],
  cache: SubtypingCache,
  level: Int,
):
  /** Get the string representation of the object. */
  override def toString(): String =
    this.clauses.map(_.show).mkString(", ")

  /** Map over the clauses of the context as a single iterator. */
  def map(f: Iterator[SubClause] => Iterator[SubClause]): SubContext =
    SubContext(f(this.clauses.iterator).toList, this.cache, this.level)

  /** Iterate over the type variable declarations. */
  def typeVarDecls: Iterator[TypeVarDecl] =
    this.clauses.iterator.flatMap(_ match
      case decl: TypeVarDecl =>
        Some(decl)
      case _ =>
        None
    )

  /** Map over the clauses of the context. */
  def mapClauses(f: SubClause => SubClause): SubContext =
    this.map(_.map(f))

  /** Map over the cache of the context.*/
  def mapCache(f: SubtypingCache => SubtypingCache): SubContext =
    SubContext(this.clauses, f(this.cache), this.level)

  /** Map over the level of the context. */
  def mapLevel(f: Int => Int): SubContext =
    SubContext(this.clauses, this.cache, f(this.level))

object SubContext:
  /** The empty subtyping context. */
  def empty =
    SubContext(Nil, SubtypingCache(), 0)

/** A typing context, which contains the term environment and the current subtyping context. */
case class TypeContext(
  sub: SubContext,
  terms: List[TermVarDecl],
):
  /** Get the string representation of the object. */
  override def toString(): String =
    (this.terms.map(_.show) ::: this.sub.clauses.map(_.show)).mkString(", ")

object TypeContext:
  /** The empty typing context. */
  def empty =
    TypeContext(SubContext.empty, Nil)
