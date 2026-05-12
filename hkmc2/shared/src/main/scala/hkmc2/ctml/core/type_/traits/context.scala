package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.core.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*

/** Handle contextual information while applying a transformation on a type. */
final class TypeContextApplicator[T[+_], P <: ContextParams[P]](
  next: TypeApplicator[T, Const[Constraint], P],
  last: TypeCombinator[T, Const[Constraint], P],
) extends TypeChainApplicator[T, Const[Constraint], P](next):
  override def apply(type_ : Type, params: P)(using first: TypeApplicator[T, Const[Constraint], P]): T[Type] =
    type_ match
      case TUniv(var_, body) =>
        val ctx = params.ctx.declVar(var_, TypeVarKind.Rigid)
        val bodyRes = first.apply(body, params.setContext(ctx))
        last.univ(var_, bodyRes, params)
      case _ =>
        next.apply(type_, params)
