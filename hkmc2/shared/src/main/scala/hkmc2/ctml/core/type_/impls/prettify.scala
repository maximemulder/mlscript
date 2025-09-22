package hkmc2.ctml.core.type_.impls

import scala.collection.mutable.HashMap as MutMap

import hkmc2.ctml.types.*
import hkmc2.ctml.util.*
import hkmc2.ctml.core.debug.output
import hkmc2.ctml.core.type_.traits.*

extension (type_ : Type)
  /** Prettify a type in a context. */
  def prettify(prettyCtx: PrettyContext): Type =
    TypePrettifier(type_, prettyCtx)

extension (clauses: Clauses)
  def prettify(prettyCtx: PrettyContext): Clauses =
    // TODO: Add a `Clauses.map` function.
    Clauses(clauses.elems.map(_.prettify(prettyCtx)))

extension (clause: Clause)
  def prettify(prettyCtx: PrettyContext): Clause =
    clause match
      case decl: TermVarDecl =>
        decl.prettify(prettyCtx)
      case decl: TypeVarDecl =>
        decl.prettify(prettyCtx)
      case bound: Bound =>
        bound.prettify(prettyCtx)

extension (decl: TermVarDecl)
  def prettify(prettyCtx: PrettyContext): TermVarDecl =
    TermVarDecl(
      decl.name,
      decl.type_.prettify(prettyCtx),
    )

extension (decl: TypeVarDecl)
  def prettify(prettyCtx: PrettyContext): TypeVarDecl =
    TypeVarDecl(
      decl.var_.prettify(prettyCtx),
      decl.kind,
      decl.original.map(_.prettify(prettyCtx)),
    )

extension (bound: Bound)
  def prettify(prettyCtx: PrettyContext): Bound =
    Bound(
      bound.var_.prettify(prettyCtx),
      bound.dir,
      bound.type_.prettify(prettyCtx)
    )

extension (var_ : TypeVar)
  def prettify(prettyCtx: PrettyContext): TypeVar =
    prettyCtx.get(var_)

extension (judgement: Judgment)
  def prettify(prettyCtx: PrettyContext): Judgment =
    judgement match
      case SubtypingJudgment(sub, sup, mode) =>
        SubtypingJudgment(
          sub.prettify(prettyCtx),
          sup.prettify(prettyCtx),
          mode,
        )
      case SupertypingJudgment(sup, sub) =>
        SupertypingJudgment(
          sub.prettify(prettyCtx),
          sup.prettify(prettyCtx),
        )
      case TypeEquivalenceJudgment(left, right) =>
        TypeEquivalenceJudgment(
          left.prettify(prettyCtx),
          right.prettify(prettyCtx),
        )
      case TypeIncomparabilityJudgment(left, right) =>
        TypeIncomparabilityJudgment(
          left.prettify(prettyCtx),
          right.prettify(prettyCtx),
        )

extension (proof: ProofTree)
  def prettify(prettyCtx: PrettyContext): ProofTree =
    ProofTree(
      proof.judgment.prettify(prettyCtx),
      proof.premises.map(_.prettify(prettyCtx))
    )

extension (error: TypeError)
  def prettify(prettyCtx: PrettyContext): TypeError =
    TypeError(
      error.message,
      error.trees.map(_.prettify(prettyCtx)),
    )

/** The list of greek letters used to prettify type variables. */
val letters = List(
  "α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ",
  "ν", "ξ", "ο", "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"
)

/** A mutable context of type variables that maps counter-based fresh variables to pretty
 *  greek letters. */
class PrettyContext(vars: MutMap[TypeVar, String] = MutMap()):
  /** Check whether a letter is used in this context. */
  def hasLetter(letter: String): Boolean =
    this.vars.values.exists(_ == letter)

  /** Get the next letter available in this context if there is one. */
  def getNextLetter(): Option[String] =
    letters.find(!this.hasLetter(_))

  /** Get the pretty alias of a type variable in this context. */
  def get(var_ : TypeVar): TypeVar =
    // Do not prettify non-fresh type variables.
    if !var_.name.matches("\\d+") then
      return var_

    // Return the pretty alias of the type variable if it is already in the context.
    this.vars.get(var_) match
      case Some(name) =>
        return TypeVar(name)
      case None =>

    // Add a pretty variable alias to the context if there is space remaining.
    this.getNextLetter() match
      case Some(letter) =>
        this.vars.addOne((var_, letter))
        TypeVar(letter)
      case None =>
        var_

object TypePrettifier extends TypeDispatcher[Id, Id, PrettyContext](TypeIdentityCombinator[PrettyContext]):
  override def apply(type_ : Type, prettyCtx: PrettyContext)(using first: TypeApplicator[Id, PrettyContext]): Type =
    type_ match
      case TVar(var_) =>
        TVar(var_.prettify(prettyCtx))
      case TUniv(var_, body) =>
        TUniv(
          var_.prettify(prettyCtx),
          this.apply(body, prettyCtx),
        )
      case _ =>
        super.apply(type_, prettyCtx)

  override def apply(bound: Bound, prettyCtx: PrettyContext): Bound =
    bound.prettify(prettyCtx)
