package hkmc2.ctml.types

import hkmc2.semantics.Statement
import scala.collection.mutable.ListBuffer

/** A CTML error. */
trait Error extends Exception

/** A CTML parsing error. */
case class ParseError(stmt: Statement) extends Error:
  override def getMessage(): String =
    s"Unsupported CTML term: ${this.stmt}"

/** A CTML typing error. */
case class TypeError(
  val message: String,
  val judgements: ListBuffer[Judgment] = ListBuffer(),
) extends Error:
  override def getMessage(): String =
    var message = this.message
    for judgment <- this.judgements do
      message += s"\n  ${judgment}"

    message

trait Judgment

case class ConstrainSubJudgment(
  val sub: Type,
  val sup: Type,
  val mode: Mode,
) extends Judgment:
  override def toString(): String =
    s"${mode.show()} ${sub.show()} ≤ ${sup.show()}"
