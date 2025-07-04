package hkmc2.ctml.types

import hkmc2.ctml.util.*

/** A typing context, which is made of an ordered list of clauses and is usualy taken as an input
 *  by various typing functions.
 */
case class Context(
  /** The list of clauses itself. */
  clauses: List[Clause] = Nil,
):
  /** Get the string representation of the object. */
  override def toString(): String =
    // TODO
    this.clauses.map(_.show).mkString(", ")

object Context:
  /** The empty typing context. */
  def none =
    Context(Nil)
