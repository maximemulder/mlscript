package hkmc2.ctml.types


import scala.collection.mutable.ListBuffer
import scala.util.control.NoStackTrace

import hkmc2.ctml.utils.*
import hkmc2.semantics.Statement


/** A CTML error. */
abstract class Error extends Exception

/** A CTML parsing error. */
case class ParseError(stmt: Statement) extends Error:
  override def getMessage(): String =
    s"Unsupported CTML term: ${this.stmt}"

/** A CTML type error.
 *
 *  Type errors are not only reported to the user, but also used for control flow during subtyping,
 *  where failing branches of a search (e.g. of a union supertype) throw a type error that is caught
 *  to try the other branches. They thus do not record the stack trace, whose capture made up about
 *  half of the subtyping time due to the depth of the subtyping recursion. Type errors are reported
 *  using their proof trees instead. */
case class TypeError(
  val message: Option[String] = None,
  var trees: List[ProofTree] = Nil,
) extends Error with NoStackTrace:
  override def getMessage(): String =
    var message = this.message match
      case Some(message) =>
        message
      case None =>
        this.trees match
          case tree :: _ =>
            s"Cannot solve judgment ${tree.judgment}."
          case Nil =>
            "Unknown type error."

    if !this.trees.isEmpty then
      message += "\nTyping error tree:\n"
      for tree <- trees do
        message += tree.show.addIndentation(1)

    message

  def addStep(judgment: Judgment) =
    this.trees = List(ProofTree(judgment, this.trees))
