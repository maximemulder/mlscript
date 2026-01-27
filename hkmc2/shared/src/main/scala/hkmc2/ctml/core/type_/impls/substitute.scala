package hkmc2.ctml.core.type_.impls.substitute

import hkmc2.ctml.core.var_.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

extension (type_ : Type)
  /** Subsitute a type variable by another type variable in the type, without simplifying the
   *  resulting type. */
  def substitute(var_ : TypeVar, substitute: TypeVar): Type =
    TypeSubstitute1(type_, TypeSubstituteParams(var_, substitute))

/** Parameters of the type substitution operation. */
class TypeSubstituteParams(
  val var_ : TypeVar,
  val substitute: TypeVar,
) extends TypeVarParams[TypeSubstituteParams]:
  def setVar(var_ : TypeVar) = TypeSubstituteParams(var_, substitute)

/** Implementation of the type substitution operation. */
object TypeSubstitute1 extends TypeShadowApplicator(TypeSubstitute2):
  override def univ(univ: TUniv): Type =
    univ

object TypeSubstitute2 extends TypeChainApplicator[Id, Id, TypeSubstituteParams](TypeSubstitute3):
  override def apply(type_ : Type, params : TypeSubstituteParams)(using first: TypeApplicator[Id, Id, TypeSubstituteParams]): Type =
    type_ match
      case TVar(var_) if var_ == params.var_ =>
        TVar(params.substitute)
      case _ =>
        next.apply(type_, params)

private def TypeSubstitute3 = TypeDispatcher[Id, Id, TypeSubstituteParams](Combinator)

private def Combinator = TypeIdentityCombinator[TypeSubstituteParams]
