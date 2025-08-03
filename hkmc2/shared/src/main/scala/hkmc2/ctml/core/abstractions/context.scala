package hkmc2.ctml.core.abstractions

import hkmc2.ctml.core.var_.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

/** Trait for objects that carry a typing context. */
trait WithContext[This <: WithContext[This]]:
  /** Get the typing context of the object. */
  def getContext: Context
  /** Set the typing context of the object. */
  def setContext(ctx: Context): This

class TypeContextApplicator[T[+_], P <: WithContext[P]](
  applicator: TypeApplicator[T, P] & BoundsApplicator[Id, P],
  combinator: TypeCombinator[T, Id, P],
) extends TypeApplicator[T, P]:
  override def apply(type_ : Type, params: P): T[Type] =
    type_ match
      case TUniv(var_, body) =>
        val ctx = params.getContext.extend(declRigidVar(var_))
        val bodyRes = this.apply(body, params.setContext(ctx))
        combinator.univ(var_, bodyRes, params)
      case TConstrained(body, bounds) =>
        val ctx = params.getContext.extend(bounds)
        val bodyRes = this.apply(body, params.setContext(ctx))
        val boundsRes = applicator.apply(bounds, params)
        combinator.constrained(bodyRes, boundsRes, params)
      case _ =>
        applicator.apply(type_, params)
