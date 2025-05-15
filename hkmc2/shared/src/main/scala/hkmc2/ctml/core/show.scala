package hkmc2.ctml.core

import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Convert the type to its string representation. */
  def show(): String =
    type_ match
      case _: TBot =>
        "⊥"
      case _: TTop =>
        "⊤"
      case var_ : TVar =>
        var_.name
      case lam: TLam =>
        s"${lam.param.show()} → ${lam.ret.show()}"
      case union: TUnion =>
        s"${union.left.show()} ∨ ${union.right.show()}"
      case inter: TInter =>
        s"${inter.left.show()} ∧ ${inter.right.show()}"
      case constraining: TConstraining =>
        s"${constraining.base.show()} ⇒ ${constraining.bounds.show()}"

extension (bounds: List[Bound])
  /** Convert a list of bounds to its string representation. */
  def show(): String =
    return bounds.map(_.show()).mkString(", ")

extension (bound: Bound)
  /** Convert the bound to its string representation. */
  def show(): String =
    return s"${bound.name} ${bound.dir.show()} ${bound.type_.show()}"

extension (dir: Direction)
  /** Convert the subtyping direction to its string representation. */
  def show(): String =
    dir match
      case Direction.Sub   => "≤"
      case Direction.Super => "≥"

extension (pol: Polarity)
  /** Convert the type polarity to its string representation. */
  def show(): String =
    pol match
      case Polarity.Negative => "−"
      case Polarity.Positive => "+"
