package hkmc2.ctml.core.subtyping

import hkmc2.ctml.config.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*

/** Check whether a judgment holds by its atoms alone, and by the hypotheses. The judgment `τ ≤ σ` is
 *  read as a clause: `τ` as the intersection of its left atoms, and `σ` as the union of its right
 *  atoms, a negation being an atom of the other side, by the complement rules (see `ClauseAtoms`).
 *  The clause holds:
 *  - When an atom is on both sides, by reflexivity: e.g. `τ ∧ σ ≤ τ ∨ ρ`. By the complement rules,
 *    this is also excluded middle, e.g. `⊤ ≤ τ ∨ ρ ∨ ¬τ`, and non-contradiction, e.g.
 *    `τ ∧ ρ ∧ ¬τ ≤ ⊥`.
 *  - When `⊥` is a left atom or `⊤` a right atom: e.g. `τ ∧ ¬⊤ ≤ σ`.
 *  - When two left atoms are disjoint constructors (see `areDisjointConstructors`): e.g.
 *    `Int ∧ τ ∧ Str ≤ ⊥`, and `⊤ ≤ ¬Int ∨ τ ∨ ¬Str`.
 *  - When it holds by these rules once the hypotheses are added to it (see `saturate`): e.g.
 *    `Str ≤ ⊥` assuming `Str ≤ Int`, since `Str ∧ Int ≤ ⊥` holds.
 *
 *  These are the rules of the mechanization that close a clause by an atom on both sides or by two
 *  disjoint atoms on the left. Unlike the choice rules, which read the operands of the outermost
 *  joint type of a side, they read all the atoms of the judgment, which may hold by two atoms that
 *  are not operands of the same joint type: e.g. `Int ∧ (τ ∧ Str) ≤ ⊥` is explored by the choice
 *  rules as `Int ≤ ⊥` or `τ ∧ Str ≤ ⊥`, which both fail.
 *
 *  This is checked before any other rule, since the other rules may turn such a judgment into
 *  judgments that are not derived. Comparing only the two sides as a whole is not enough, since the
 *  rules that are applied before the choice rules may not keep their operands as they are: e.g.
 *  `({c} ⟹ α) ∧ τ ≤ {c} ⟹ α` opens the right constrained type before choosing the left operand of
 *  the subtype (see `subtypeImpl`), and then requires `{c} ⟹ α ≤ α`, which is not derived when `α`
 *  is rigid: `α` is replaced by its lower bound before the left constrained type is opened. */
def holdsByAtoms(sub: Type, sup: Type)(using ctx: SubContext): Boolean =
  val hypotheses = if config.hypothesis then ctx.hypotheses.judgments.toList else Nil
  ClauseAtoms(Nil, Nil).add(sub, Polarity.Negative).add(sup, Polarity.Positive).saturate(hypotheses).isClosed

/** The atoms of a clause, as read from a subtyping judgment (see `holdsByAtoms`). */
private case class ClauseAtoms(
  /** The left atoms, whose intersection is the subtype. */
  val left: List[Type],
  /** The right atoms, whose union is the supertype. */
  val right: List[Type],
):
  /** Add a type and its atoms to the clause: to its left side at the negative polarity, and to its
   *  right side at the positive polarity.
   *
   *  The atoms of a side are the operands of its nested intersections on the left, and of its nested
   *  unions on the right. The body of a negation is an atom of the other side, since `τ ∧ ¬ρ ≤ σ` iff
   *  `τ ≤ ρ ∨ σ`, and `τ ≤ ¬ρ ∨ σ` iff `τ ∧ ρ ≤ σ`, by the complement rules (see `negation.scala`).
   *  The types that are decomposed are atoms as well, so that a clause may also be closed by an
   *  operand that is itself a joint type or a negation: e.g. `τ ∨ ρ ≤ (τ ∨ ρ) ∨ σ`. */
  def add(type_ : Type, pol: Polarity): ClauseAtoms =
    val atoms = pol match
      case Polarity.Negative =>
        ClauseAtoms(type_ :: this.left, this.right)
      case Polarity.Positive =>
        ClauseAtoms(this.left, type_ :: this.right)
    type_ match
      case TNeg(body) =>
        atoms.add(body, !pol)
      case TJointType(mode, left, right) if mode.isNaturalPol(pol) =>
        atoms.add(left, pol).add(right, pol)
      case _ =>
        atoms

  /** Check whether the clause holds by its atoms alone, without the hypotheses (see
   *  `holdsByAtoms`). */
  def isClosed(using ctx: SubContext): Boolean =
    this.left.exists(this.right.contains) || this.left.contains(TBot) || this.right.contains(TTop) ||
      this.left.combinations(2).exists((pair) => areDisjointConstructors(pair(0), pair(1)))

  /** Add the hypotheses to the clause, each as a cut, until the clause is closed or no hypothesis is
   *  left to add (see `cut`). */
  def saturate(hypotheses: List[(Type, Type)])(using ctx: SubContext): ClauseAtoms =
    if this.isClosed then
      return this

    hypotheses.iterator.flatMap((hypothesis) => this.cut(hypothesis).map((hypothesis, _))).nextOption() match
      case Some(hypothesis, atoms) =>
        atoms.saturate(hypotheses.filterNot(_ == hypothesis))
      case None =>
        this

  /** Get the clause that this clause follows from by a cut on a hypothesis `l ≤ r`, if one of the two
   *  premises of the cut is closed: from `l ≤ r`, the clause `P ≤ N` follows from `P ≤ N ∨ l` and
   *  `P ∧ r ≤ N`, so that it is `P ∧ r ≤ N` if `P ≤ N ∨ l` is closed, and `P ≤ N ∨ l` if
   *  `P ∧ r ≤ N` is closed.
   *
   *  This uses a hypothesis by transitivity, which the hypothesis rule, an exact match, does not. A
   *  hypothesis is never a bound, whose variable rules already read it from the context, so this is
   *  the lookup of the bounds of a variable extended to the other atoms, which the mechanization
   *  parks ("a lookup keyed on a side that is not a variable"), restricted to the closing rules, as
   *  its lookups are offered to the closing rules first. Adding atoms never opens a clause, so
   *  that the hypotheses may be added in any order, each at most once. */
  private def cut(hypothesis: (Type, Type))(using ctx: SubContext): Option[ClauseAtoms] =
    val (sub, sup) = hypothesis
    if this.add(sub, Polarity.Positive).isClosed then
      Some(this.add(sup, Polarity.Negative))
    else if this.add(sup, Polarity.Negative).isClosed then
      Some(this.add(sub, Polarity.Positive))
    else
      None
