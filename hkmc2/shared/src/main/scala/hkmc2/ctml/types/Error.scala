package hkmc2.ctml.types

import hkmc2.semantics.Statement
import scala.collection.mutable.ListBuffer

/** A CTML error. */
abstract class Error extends Exception

/** A CTML parsing error. */
case class ParseError(stmt: Statement) extends Error:
  override def getMessage(): String =
    s"Unsupported CTML term: ${this.stmt}"

case class TypingTree(
  val sub: Type,
  val sup: Type,
  val premises: List[TypingTree],
):
  def show(level: Int): String =
    var tree = s"\n${"  " * level}${this.sub} ≤ ${this.sup}"
    for premise <- this.premises do
      tree += premise.show(level + 1)

    tree

/** A CTML type error. */
case class TypeError(
  val message: Option[String] = None,
  var trees: List[TypingTree] = Nil,
) extends Error:
  override def getMessage(): String =
    var message = this.message match
      case Some(message) =>
        message
      case None =>
        this.trees match
          case tree :: _ =>
            s"Cannot solve type equation ${tree.sub} ≤ ${tree.sup}."
          case Nil =>
            "Unknown type error."

    if !this.trees.isEmpty then
      message += "\nTyping error tree:"
      for tree <- trees do
        message += tree.show(1)

    message

  def addStep(sub: Type, sup: Type) =
    this.trees = List(TypingTree(sub, sup, premises = trees))
