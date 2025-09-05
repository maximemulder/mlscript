package hkmc2.ctml.types

import scala.collection.mutable.HashMap as MutMap
import hkmc2.ctml.core.debug.DebugInfo.output

/** Convert the type to its string representation. */
def showType(type_ : Type, parentOpen: Boolean = false): String =
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

def showTypeVar(var_ : TypeVar): String =
  var_.name

/** Convert a list of type variables to its string representation. */
def showTypeVars(vars: List[TypeVar]): String =
  vars.map(_.name).mkString(", ")

/** Implementation of the `Show` trait for `Clause`. */
def showClause(clause: Clause): String =
  clause match
    case var_ : TermVarDecl =>
      showTermVarDecl(var_)
    case var_ : TypeVarDecl =>
      showTypeVarDecl(var_)
    case bound: Bound =>
      showBound(bound)

/** Implementation of the `Show` trait for `TermVarDecl`. */
def showTermVarDecl(decl : TermVarDecl): String =
  s"${decl.name}: ${showType(decl.type_)}"

/** Implementation of the `Show` trait for `TypeVarDecl`. */
def showTypeVarDecl(decl : TypeVarDecl): String =
  s"${showTypeVar(decl.var_)} ${decl.kind}"

/** Implementation of the `Show` trait for `Bound`. */
def showBound(bound: Bound): String =
  s"${showTypeVar(bound.var_)} ${bound.dir} ${showType(bound.type_)}"

/** Convert a list of bounds to its string representation. */
def showBounds(bounds: List[Bound]): String =
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
