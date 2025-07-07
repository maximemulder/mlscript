package hkmc2.ctml.types

import hkmc2.ctml.util.*

/** A type. */
sealed trait Type:
  /** Get the string representation of the object. */
  override def toString: String =
    this.showType()

/** The bottom type. */
case object TBot extends Type

/** The top type. */
case object TTop extends Type

/** A type variable type. */
case class TVar(val var_ : TypeVar) extends Type

/** A lambda type. */
case class TLam(val param: Type, val ret: Type) extends Type

/** A union type. */
case class TUnion(val left: Type, val right: Type) extends Type

/** An intersection type. */
case class TInter(val left: Type, val right: Type) extends Type

/** A constrained type. */
case class TConstrained(val vars: List[TypeVar], val base: Type, val bounds: List[Bound]) extends Type

/** A constraing type. */
case class TConstraining(val base: Type, val bounds: List[Bound]) extends Type

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
      case TLam(param, ret) =>
        List(param, ret)
      case TUnion(left, right) =>
        List(left, right)
      case TInter(left, right) =>
        List(left, right)
      case TConstrained(_, base, bounds) =>
        bounds.map(_.type_) :+ base
      case TConstraining(base, bounds) =>
        bounds.map(_.type_) :+ base

/** Implementation of the `Tree` trait for `Type`. */
implicit def TypeTree: Tree[Type] = new Tree {
  override def children(type_ : Type): List[Type] =
    type_.components
}

/** Implementation of the `Show` trait for `Type`. */
implicit def TypeShow: Show[Type] = new Show {
  override def show(type_ : Type): String =
    type_.showType()
}

extension (type_ : Type)
    /** Convert the type to its string representation. */
  private def showType(parentOpen: Boolean = false): String =
    val (string, selfOpen) = type_ match
      case _: TBot =>
        ("⊥", false)
      case _: TTop =>
        ("⊤", false)
      case TVar(var_) =>
        (var_.show, false)
      case TLam(param, ret) =>
        val components = param :: ret.getLambdaComponents()
        (components.map(_.showType(true)).mkString(" → "), true)
      case TUnion(left, right) =>
        val components = left :: right.getUnionComponents()
        (components.map(_.showType(true)).mkString(" ∨ "), true)
      case TInter(left, right) =>
        val components = left :: right.getInterComponents()
        (components.map(_.showType(true)).mkString(" ∧ "), true)
      case constrained: TConstrained =>
        var baseString = s"∀${showTypeVars(constrained.vars.reverse)}"
        if constrained.bounds != Nil then
          baseString += s" ◁ {${showBounds(constrained.bounds)}}"
        baseString += s". ${constrained.base.showType(false)}"
        (baseString, true)
      case constraining: TConstraining =>
        (s"${constraining.base.showType(false)} ▷ {${showBounds(constraining.bounds)}}", true)

    // If the type is surrounded by spaces in its parent, and has spaces itself, add parentheses
    // around it.
    if parentOpen && selfOpen then
      s"(${string})"
    else
      string

  /** Get the right-recursive components of a lambda type. */
  private def getLambdaComponents(): List[Type] =
    type_ match
      case TLam(param, ret) =>
        param :: ret.getInterComponents()
      case _ =>
        type_ :: Nil

  /** Get the right-recursive components of an union type. */
  private def getUnionComponents(): List[Type] =
    type_ match
      case TUnion(left, right) =>
        left :: right.getUnionComponents()
      case _ =>
        type_ :: Nil

  /** Get the right-recursive components of an intersection type. */
  private def getInterComponents(): List[Type] =
    type_ match
      case TInter(left, right) =>
        left :: right.getInterComponents()
      case _ =>
        type_ :: Nil
