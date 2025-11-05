package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.core.var_.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

/** Trait for objects that carry a typing context. */
trait WithContext[This <: WithContext[This]]:
  /** Get the typing context of the object. */
  def getContext: Context
  /** Set the typing context of the object. */
  def setContext(ctx: Context): This

/** Handle contextual information while applying a transformation on a type. */
class TypeContextApplicator[T[+_], P <: WithContext[P]](
  next: TypeApplicator[T, P] & BoundsApplicator[Id, P] & TypeNode[T, Id, P],
) extends TypeApplicator[T, P], TypeNode[T, Id, P]:
  def getCombinator: TypeCombinator[T, Id, P] = next.getCombinator

  override def apply(type_ : Type, params: P)(using first: TypeApplicator[T, P]): T[Type] =
    type_ match
      case TUniv(var_, body) =>
        val ctx = params.getContext.extend(declRigidVar(var_))
        val bodyRes = first.apply(body, params.setContext(ctx))
        next.getCombinator.univ(var_, bodyRes, params)
      case TConstrained(body, bounds) =>
        val ctx = params.getContext.extend(bounds)
        val bodyRes = first.apply(body, params.setContext(ctx))
        // TODO: This is very ugly.
        val boundsRes = next.apply(List(bounds), params)
        next.getCombinator.constrained(bodyRes, boundsRes(0), params)
      case _ =>
        next.apply(type_, params)
