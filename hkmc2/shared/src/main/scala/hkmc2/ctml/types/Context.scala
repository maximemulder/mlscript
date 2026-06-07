package hkmc2.ctml.types

import hkmc2.ctml.utils.*
import hkmc2.ctml.core.subtyping.SubtypingCache

/** A typing context, which is made of an ordered list of clauses and is usualy taken as an input
 *  by various typing functions.
 */
case class Context(
  /** The list of clauses itself. */
  clauses: List[Clause],
  cache: SubtypingCache,
  level: Int,
):
  /** Get the string representation of the object. */
  override def toString(): String =
    this.clauses.map(_.show).mkString(", ")

  /** Map over the clauses of the context as a single iterator. */
  def map(f: Iterator[Clause] => Iterator[Clause]): Context =
    Context(f(this.clauses.iterator).toList, this.cache, this.level)

  /** Iterate over the type variable declarations. */
  def typeVarDecls: Iterator[TypeVarDecl] =
    this.clauses.iterator.flatMap(_ match
      case decl: TypeVarDecl =>
        Some(decl)
      case _ =>
        None
    )

  /** Map over the clauses of the context. */
  def mapClauses(f: Clause => Clause): Context =
    this.map(_.map(f))

  /** Map over the cache of the context.*/
  def mapCache(f: SubtypingCache => SubtypingCache): Context =
    Context(this.clauses, f(this.cache), this.level)

  /** Map over the level of the context. */
  def mapLevel(f: Int => Int): Context =
    Context(this.clauses, this.cache, f(this.level))

object Context:
  /** The empty typing context. */
  def empty =
    Context(Nil, SubtypingCache(), 0)
