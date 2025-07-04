package hkmc2.ctml.types

import hkmc2.ctml.util.*

/** The typing mode. */
enum Mode:
  /** No new constraints should be genereated to get a positive result. */
  case Check
  /** New constraints can be genereated to get a positive result. */
  case Constrain

  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** Implementation of the `Show` trait for `Mode`. */
implicit def ModeShow: Show[Mode] = new Show {
  override def show(mode: Mode): String =
    mode match
      case Mode.Constrain => "constrain"
      case Mode.Check     => "check"
}
