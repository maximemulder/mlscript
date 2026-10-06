package hkmc2.ctml.types

import hkmc2.ctml.utils.*
import hkmc2.ctml.core.subtyping.{Hypotheses, SubtypingTrail}

/** A subtyping context, which contains the type-level information used by subtyping,
 *  simplification, and level solving.
 */
case class SubContext(
  /** The list of clauses itself. */
  clauses: List[SubClause],
  /** The judgments in progress on the current path of the subtyping search. */
  trail: SubtypingTrail,
  /** The judgments assumed on the current path of the subtyping search that are not bounds. */
  hypotheses: Hypotheses,
  /** The current polymorphism level. */
  level: Int,
):
  /** Get the string representation of the object. */
  override def toString(): String =
    this.clauses.map(_.show).mkString(", ")

  /** Map over the clauses of the context as a single iterator. */
  def map(f: Iterator[SubClause] => Iterator[SubClause]): SubContext =
    SubContext(f(this.clauses.iterator).toList, this.trail, this.hypotheses, this.level)

  /** Iterate over the type variable declarations. */
  def typeVarDecls: Iterator[TypeVarDecl] =
    this.clauses.iterator.flatMap(_ match
      case decl: TypeVarDecl =>
        Some(decl)
      case _ =>
        None
    )

  /** Map over the clauses of the context. */
  def mapClauses(f: SubClause => SubClause): SubContext =
    this.map(_.map(f))

  /** Map over the trail of the context. */
  def mapTrail(f: SubtypingTrail => SubtypingTrail): SubContext =
    SubContext(this.clauses, f(this.trail), this.hypotheses, this.level)

  /** Map over the hypotheses of the context. */
  def mapHypotheses(f: Hypotheses => Hypotheses): SubContext =
    SubContext(this.clauses, this.trail, f(this.hypotheses), this.level)

  /** Map over the level of the context. */
  def mapLevel(f: Int => Int): SubContext =
    SubContext(this.clauses, this.trail, this.hypotheses, f(this.level))

object SubContext:
  /** The empty subtyping context. */
  def empty =
    SubContext(Nil, SubtypingTrail(), Hypotheses(), 0)

/** A typing context, which contains the term environment and the current subtyping context. */
case class TypeContext(
  sub: SubContext,
  terms: List[TermVarDecl],
):
  /** Get the string representation of the object. */
  override def toString(): String =
    (this.terms.map(_.show) ::: this.sub.clauses.map(_.show)).mkString(", ")

object TypeContext:
  /** The empty typing context. */
  def empty =
    TypeContext(SubContext.empty, Nil)
