package hkmc2.ctml.core.abstractions

import hkmc2.ctml.types.*

/** Trait for objects that carry a type variable. */
trait WithTypeVar[This <: WithTypeVar[This]]:
  /** Get the type variable of the object. */
  def getTypeVar: TypeVar
  /** Set the type variable of the object. */
  def setTypeVar(var_ : TypeVar): This

// TODO: Find a way to make this modulable.
// Current best guess: (full applicator, next applicator, end combinator).
trait TypeVarDispatcher[T[+_], B[+_], P <: WithTypeVar[P]] extends TypeDispatcher[T, B, P]:
  override def apply(type_ : Type, params: P): T[Type] =
    type_ match
      case TUniv(var_, body) if var_ == params.getTypeVar =>
        this.apply(TUniv(var_, body))
      case _ =>
        super.apply(type_, params)

  def apply(univ: TUniv): T[Type]
