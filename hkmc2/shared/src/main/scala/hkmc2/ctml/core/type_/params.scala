package hkmc2.ctml.core.type_

import hkmc2.ctml.types.*

/** Trait for objects that carry a subtyping context. */
trait ContextParams[This <: ContextParams[This]]:
  /** The subtyping context of the parameters. */
  val ctx: SubContext
  /** Set the subtyping context of the parameters. */
  def setContext(ctx: SubContext): This

/** Trait for objects that carry a polarity. */
trait PolarityParams[This <: PolarityParams[This]]:
  /** The polarity of the parameters. */
  val pol: Polarity
  /** Set the polarity of the parameters. */
  def setPolarity(pol: Polarity): This

/** Trait for objects that carry a type variable. */
trait TypeVarParams[This <: TypeVarParams[This]]:
  /** The type variable of the parameters. */
  val var_ : TypeVar
  /** Set the type variable of the parameters. */
  def setVar(var_ : TypeVar): This
