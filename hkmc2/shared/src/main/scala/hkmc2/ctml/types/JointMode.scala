package hkmc2.ctml.types

/** A joint type mode. */
enum JointMode:
  /** The union type mode. */
  case Union
  /** The intersection type mdoe. */
  case Inter

  /** Get the string representation of the object. */
  override def toString: String =
    this match
      case Union => "union"
      case Inter => "inter"

  /** Invert the join type mode. */
  def unary_! : JointMode =
    this match
      case Union => JointMode.Inter
      case Inter => JointMode.Union
