package hkmc2.ctml.types

import hkmc2.semantics.Term

/** A CTML error. */
trait Error extends Exception

/** A CTML parsing error. */
case class ParseError(term: Term) extends Error:
  override def getMessage(): String =
    s"Unsupported CTML term: ${this.term}"

/** A CTML typing error. */
case class TypeError(message: String) extends Error:
  override def getMessage(): String =
    this.message
