package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*

/** Handle variable shadowing while applying a transformation on a type. */
trait TypeShadowApplicator[T[+_], P <: TypeVarParams[P]](
  val next: TypeApplicator[T, P]
) extends TypeApplicator[T, P]:
  override def apply(type_ : Type, params: P)(using first: TypeApplicator[T, P]): T[Type] =
    type_ match
      case TUniv(var_, body) if var_ == params.var_ =>
        this.univ(TUniv(var_, body))
      case _ =>
        next.apply(type_, params)

  /** Apply the transformation on a variable shadowing universal type. */
  def univ(univ: TUniv): T[Type]
