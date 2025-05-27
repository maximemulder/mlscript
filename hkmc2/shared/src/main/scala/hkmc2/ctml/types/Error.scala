package hkmc2.ctml.types

import hkmc2.semantics.Statement
import scala.collection.mutable.ListBuffer

/** A CTML error. */
abstract class Error extends Exception

/** A CTML parsing error. */
class ParseError(stmt: Statement) extends Error:
  override def getMessage(): String =
    s"Unsupported CTML term: ${this.stmt}"

/** A CTML type error. */
class TypeError(
  val steps: ListBuffer[(Type, Type)] = ListBuffer(),
) extends Error:
  override def getMessage(): String =
    if this.steps.isEmpty then
      return s"Unknown type error."

    val step = this.steps(0)
    var message = s"Cannot solve type equation ${step._1} ≤ ${step._2}."
    message += this.getTypingTrace()
    message

  def getTypingTrace(): String =
    if this.steps.isEmpty then
      return ""

    var message = "\nTyping trace:"
    for step <- steps.reverse do
      message += s"\n  ${step._1} ≤ ${step._2}"

    message

/** A CTML type error with a custom message. */
case class TypeMessageError(
  val message: String,
) extends TypeError:
  override def getMessage(): String =
    var message = this.message
    message += this.getTypingTrace()
    message
