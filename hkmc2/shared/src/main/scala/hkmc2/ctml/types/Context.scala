package hkmc2.ctml.types

import hkmc2.ctml.utils.*

/** A typing context, which is made of an ordered list of clauses and is usualy taken as an input
 *  by various typing functions.
 */
case class Context(
  /** The list of clauses itself. */
  clauses: List[Clause],
):
  /** Get the string representation of the object. */
  override def toString(): String =
    this.clauses.map(_.show).mkString(", ")

  /** Map over the clauses of the context as a single iterator. */
  def map(f: Iterator[Clause] => Iterator[Clause]): Context =
    Context(f(this.clauses.iterator).toList)

  /** Map over the clauses of the context. */
  def mapClauses(f: Clause => Clause): Context =
    this.map(_.map(f))

  /** Iterate over the type variable declarations. */
  def typeVarDecls: Iterator[TypeVarDecl] =
    this.clauses.iterator.flatMap(_ match
      case decl: TypeVarDecl =>
        Some(decl)
      case _ =>
        None
    )

object Context:
  /** The empty typing context. */
  def empty =
    Context(Nil)
