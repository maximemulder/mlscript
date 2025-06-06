package hkmc2.ctml.types

/** A proof tree. */
case class ProofTree(val judgment: Judgment, val premises: List[ProofTree]):
  /** Convert the proof tree to a string. */
  def show(level: Int): String =
    var tree = s"\n${"  " * level}${judgment.show()}"
    for premise <- this.premises do
      tree += premise.show(level + 1)

    tree

  /** Get the string representation of the object. */
  override def toString(): String =
    this.show(0)

/** A proof judgment. */
abstract trait Judgment:
  /** Convert the judgment to a string. */
  def show(): String

  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()

/** A subtyping judgment, which states that one type is subtype of another type. */
case class SubtypingJudgment(val sub: Type, val sup: Type) extends Judgment:
  /** Convert the subtype judgment to a string. */
  override def show(): String =
    s"${this.sub} ≤ ${this.sup}"

/** A supertyping judgment, which states that one type is supertype of another type. */
case class SupertypingJudgment(val sup: Type, val sub: Type) extends Judgment:
  /** Convert the supertype judgment to a string. */
  override def show(): String =
    s"${this.sub} ≥ ${this.sup}"

/** A type equivalence judgment, which states that two types are equivalent. */
case class TypeEquivalenceJudgment(val left: Type, val right: Type) extends Judgment:
  /** Convert the type equivalence judgment to a string. */
  override def show(): String =
    s"${this.left} ≡ ${this.right}"

/** A type incomparability judgement, which states that two types are neither subtypes nor
 *  supertypes of each other. */
case class TypeIncomparabilityJudgment(val left: Type, val right: Type) extends Judgment:
  /** Convert the type incomparability judgment to a string. */
  override def show(): String =
    s"${this.left} ≹ ${this.right}"
