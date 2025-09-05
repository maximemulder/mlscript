package hkmc2.ctml.types

import hkmc2.ctml.util.*

/** A proof tree. */
case class ProofTree(val judgment: Judgment, val premises: List[ProofTree]):
  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** Implementation of the `Tree` trait for `ProofTree`. */
given Tree[ProofTree] with
  override def children(tree: ProofTree): List[ProofTree] =
    tree.premises

/** Implementation of the `Show` trait for `ProofTree`. */
given Show[ProofTree] =
  given Show[ProofTree] with
    override def show(tree: ProofTree): String =
      tree.judgment.show()

  TreeShow

/** A proof judgment. */
sealed trait Judgment:
  /** Convert the judgment to a string. */
  def show(): String

  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()

/** A subtyping judgment, which states that one type is subtype of another type. */
case class SubtypingJudgment(val sub: Type, val sup: Type, val mode: Mode) extends Judgment:
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
