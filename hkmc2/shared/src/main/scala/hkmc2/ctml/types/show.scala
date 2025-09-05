package hkmc2.ctml.types

import scala.collection.mutable.HashMap as MutMap
import hkmc2.ctml.core.debug.DebugInfo.output

/** The list of greek letters used to display type variables. */
val letters = List(
  "α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ",
  "ν", "ξ", "ο", "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"
)

/** Printer that handles the pretty printing of types by renaming fresh variables to pretty
 *  letters. */
class TypePrinter(aliases: MutMap[String, String] = MutMap()):
  /** Check whether a letter is used in the scope. */
  def hasLetter(letter: String): Boolean =
    this.aliases.values.exists(_ == letter)

  /** Get the next available letter available in the scope. */
  def getNextLetter(): Option[String] =
    letters.find(!this.hasLetter(_))

  /** Add a binding to the scope. */
  def addBinding(binding: String): Unit =
    if !binding.matches("\\d+") then
      return

    this.getNextLetter() match
      case Some(letter) =>
        this.aliases.addOne((binding, letter))
      case None =>
        ()

  /** Get the alias of a binding if there is one. */
  def getAlias(binding: String): String =
    this.aliases.get(binding) match
      case Some(name) =>
        name
      case None =>
        binding

/** Convert the type to its string representation. */
def showType(type_ : Type, parentOpen: Boolean = false)(using scope: TypePrinter): String =
  val (string, selfOpen) = type_ match
    case _: TBot =>
      ("⊥", false)
    case _: TTop =>
      ("⊤", false)
    case TVar(var_) =>
      (showTypeVar(var_), false)
    case TTuple(left, right) =>
      (s"⟨${showType(left)}, ${showType(right)}⟩", false)
    case TLam(param, ret) =>
      val components = param :: getLambdaComponents(ret)
      (components.map(showType(_, true)).mkString(" → "), true)
    case TUnion(left, right) =>
      val components = left :: getUnionComponents(right)
      (components.map(showType(_, true)).mkString(" ∨ "), true)
    case TInter(left, right) =>
      val components = left :: getInterComponents(right)
      (components.map(showType(_, true)).mkString(" ∧ "), true)
    case TApp(abs, arg) =>
      (s"${showType(abs)}[${showType(arg)}]", false)
    case univ: TUniv =>
      val (vars, body) = getUnivComponents(univ)
      for var_ <- vars do
        scope.addBinding(var_.name)
      (s"∀${showTypeVars(vars)}. ${showType(body)}", true)
    case TConstrained(body, bounds) =>
      (s"{${showBounds(bounds)}} ⟹ ${showType(body)}", true)
    case TConstraining(body, bounds) =>
      (s"${showType(body)} ⟹ {${showBounds(bounds)}}", true)

  // If the type is surrounded by spaces in its parent, and has spaces itself, add parentheses
  // around it.
  if parentOpen && selfOpen then
    s"(${string})"
  else
    string

def showTypeVar(var_ : TypeVar)(using scope: TypePrinter): String =
  scope.getAlias(var_.name)

/** Convert a list of type variables to its string representation. */
def showTypeVars(vars: List[TypeVar])(using scope: TypePrinter): String =
  vars.map(var_ => scope.getAlias(var_.name)).mkString(", ")

/** Implementation of the `Show` trait for `Clause`. */
def showClause(clause: Clause)(using scope: TypePrinter): String =
  clause match
    case var_ : TermVarDecl =>
      showTermVarDecl(var_)
    case var_ : TypeVarDecl =>
      showTypeVarDecl(var_)
    case bound: Bound =>
      showBound(bound)

/** Implementation of the `Show` trait for `TermVarDecl`. */
def showTermVarDecl(decl : TermVarDecl)(using scope: TypePrinter): String =
  s"${decl.name}: ${showType(decl.type_)}"

/** Implementation of the `Show` trait for `TypeVarDecl`. */
def showTypeVarDecl(decl : TypeVarDecl)(using scope: TypePrinter): String =
  s"${showTypeVar(decl.var_)} ${decl.kind}"

/** Implementation of the `Show` trait for `Bound`. */
def showBound(bound: Bound)(using scope: TypePrinter): String =
  s"${showTypeVar(bound.var_)} ${bound.dir} ${showType(bound.type_)}"

/** Convert a list of bounds to its string representation. */
def showBounds(bounds: List[Bound])(using scope: TypePrinter): String =
  bounds.reverse.map(showBound(_)).mkString(", ")

/** Get the right-recursive nested components of a lambda type. */
private def getLambdaComponents(type_ : Type): List[Type] =
  type_ match
    case TLam(param, ret) =>
      param :: getLambdaComponents(ret)
    case _ =>
      type_ :: Nil

/** Get the right-recursive nested components of an union type. */
private def getUnionComponents(type_ : Type): List[Type] =
  type_ match
    case TUnion(left, right) =>
      left :: getUnionComponents(right)
    case _ =>
      type_ :: Nil

/** Get the right-recursive nested components of an intersection type. */
private def getInterComponents(type_ : Type): List[Type] =
  type_ match
    case TInter(left, right) =>
      left :: getInterComponents(right)
    case _ =>
      type_ :: Nil

/** Get the nested components of a universal type. */
private def getUnivComponents(type_ : Type): (List[TypeVar], Type) =
  type_ match
    case TUniv(var_, body) =>
      val (nestedVars, nestedBody) = getUnivComponents(body)
      (var_ :: nestedVars, nestedBody)
    case _ =>
      (Nil, type_)
