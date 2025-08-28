package hkmc2.ctml.core.abstractions

import hkmc2.ctml.types.*

/** Trait for objects that carry a type variable. */
trait WithTypeVar[This <: WithTypeVar[This]]:
  /** Get the type variable of the object. */
  def getTypeVar: TypeVar
  /** Set the type variable of the object. */
  def setTypeVar(var_ : TypeVar): This

/** Handle variable shadowing while applying a transformation on a type. */
trait TypeShadowApplicator[T[+_], P <: WithTypeVar[P]](
  next: TypeApplicator[T, P]
) extends TypeApplicator[T, P]:
  override def apply(type_ : Type, params: P)(using first: TypeApplicator[T, P]): T[Type] =
    type_ match
      case TUniv(var_, body) if var_ == params.getTypeVar =>
        this.univ(TUniv(var_, body))
      case _ =>
        next.apply(type_, params)

  /** Apply the transformation on a variable shadowing universal type. */
  def univ(univ: TUniv): T[Type]
