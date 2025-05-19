package hkmc2.ctml.types

/** The typing mode. */
enum Mode:
  /** No new constraints should be genereated to get a positive result. */
  case Check
  /** New constraints can be genereated to get a positive result. */
  case Constrain

  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()
