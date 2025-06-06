package hkmc2.ctml.types

import hkmc2.semantics.Statement
import scala.collection.mutable.ListBuffer

/** A CTML error. */
abstract class Error extends Exception

/** A CTML parsing error. */
case class ParseError(stmt: Statement) extends Error:
  override def getMessage(): String =
    s"Unsupported CTML term: ${this.stmt}"

/** A CTML type error. */
case class TypeError(
  val message: Option[String] = None,
  var trees: List[ProofTree] = Nil,
) extends Error:
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
      message += "\nTyping error tree:"
      for tree <- trees do
        message += tree.show(1)

    message

  def addStep(judgment: Judgment) =
    this.trees = List(ProofTree(judgment, this.trees))
