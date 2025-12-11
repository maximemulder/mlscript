package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.core.var_.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

/** Handle contextual information while applying a transformation on a type. */
class TypeContextApplicator[T[+_], P <: ContextParams[P]](
  next: TypeApplicator[T, P] & BoundApplicator[Id, P] & TypeNode[T, Id, P],
) extends TypeApplicator[T, P], TypeNode[T, Id, P]:
  def getCombinator: TypeCombinator[T, Id, P] = next.getCombinator

  override def apply(type_ : Type, params: P)(using first: TypeApplicator[T, P]): T[Type] =
    type_ match
      case TUniv(var_, body) =>
        val ctx = params.ctx.extend(declRigidVar(var_))
        val bodyRes = first.apply(body, params.setContext(ctx))
        next.getCombinator.univ(var_, bodyRes, params)
      case TConstrained(body, bound) =>
        val ctx = params.ctx.extend(bound)
        val bodyRes = first.apply(body, params.setContext(ctx))
        val boundRes = next.apply(bound, params)
        next.getCombinator.constrained(bodyRes, boundRes, params)
      case _ =>
        next.apply(type_, params)
