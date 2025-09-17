package hkmc2.ctml.types

import hkmc2.ctml.util.*
import hkmc2.ctml.util.given

/** A type. */
sealed trait Type:
  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** The bottom type. */
case object TBot extends Type

/** The top type. */
case object TTop extends Type

/** A type variable type. */
case class TVar(val var_ : TypeVar) extends Type

/** A tuple typle. */
case class TTuple(val left: Type, val right: Type) extends Type

/** A lambda type. */
case class TLam(val param: Type, val ret: Type) extends Type

/** A union type. */
case class TUnion(val left: Type, val right: Type) extends Type

/** An intersection type. */
case class TInter(val left: Type, val right: Type) extends Type

/** A type application. */
case class TApp(val abs: Type, val arg: Type) extends Type

/** A universal quantification type. */
case class TUniv(val var_ : TypeVar, val body: Type) extends Type

/** A constrained type. */
case class TConstrained(val body: Type, val bounds: List[Bound]) extends Type

/** A constraining type. */
case class TConstraining(val body: Type, val bounds: List[Bound]) extends Type

/** The top type type alias. */
type TBot = TBot.type

/** The bottom type type alias. */
type TTop = TTop.type

extension (type_ : Type)
  /** Get the components of a type. */
  def components: List[Type] =
    type_ match
      case TBot | TTop | TVar(_) =>
        Nil
      case TTuple(left, right) =>
        List(left, right)
      case TLam(param, ret) =>
        List(param, ret)
      case TUnion(left, right) =>
        List(left, right)
      case TInter(left, right) =>
        List(left, right)
      case TApp(abs, arg) =>
        List(abs, arg)
      case TUniv(_, body) =>
        List(body)
      case TConstrained(body, bounds) =>
        bounds.map(_.type_) :+ body
      case TConstraining(body, bounds) =>
        bounds.map(_.type_) :+ body

/** Implementation of the `Tree` trait for `Type`. */
given Tree[Type] with
  override def children(type_ : Type): List[Type] =
    type_.components

/** Implementation of the `Show` trait for `Type`. */
given Show[Type] with
  override def show(type_ : Type): String =
    showType(type_)

/** Convert the type to its string representation. */
private def showType(type_ : Type, parentOpen: Boolean = false): String =
  val (string, selfOpen) = type_ match
    case _: TBot =>
      ("⊥", false)
    case _: TTop =>
      ("⊤", false)
    case TVar(var_) =>
      (var_.show, false)
    case TTuple(left, right) =>
      (s"⟨${showType(left)}, ${showType(right)}⟩", false)
    case lambda: TLam =>
      (lambda.getLambdaComponents.map(showType(_, true)).mkString(" → "), true)
    case union: TUnion =>
      (union.getUnionComponents.map(showType(_, true)).mkString(" ∨ "), true)
    case inter: TInter =>
      (inter.getInterComponents.map(showType(_, true)).mkString(" ∧ "), true)
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

/** Convert a list of type variables to its string representation. */
private def showTypeVars(vars: List[TypeVar]): String =
  vars.map(_.name).mkString(", ")

/** Convert a list of bounds to its string representation. */
private def showBounds(bounds: List[Bound]): String =
  bounds.reverse.map(_.show).mkString(", ")

extension (type_ : Type)
  /** Get the right-recursive nested lambda components of the type. */
  def getLambdaComponents: List[Type] =
    type_ match
      case TLam(param, ret) =>
        param :: ret.getLambdaComponents
      case _ =>
        type_ :: Nil

  /** Get the right-recursive nested union components of the type. */
  def getUnionComponents: List[Type] =
    type_ match
      case TUnion(left, right) =>
        left :: right.getUnionComponents
      case _ =>
        type_ :: Nil

  /** Get the right-recursive nested intersection components of the type. */
  def getInterComponents: List[Type] =
    type_ match
      case TInter(left, right) =>
        left :: right.getInterComponents
      case _ =>
        type_ :: Nil

  /** Get the nested universal components of the type. */
  def getUnivComponents: (List[TypeVar], Type) =
    type_ match
      case TUniv(var_, body) =>
        val (nestedVars, nestedBody) = body.getUnivComponents
        (var_ :: nestedVars, nestedBody)
      case _ =>
        (Nil, type_)
