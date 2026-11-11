package hkmc2.ctml.core.type_.impls.substitute

import hkmc2.ctml.core.var_.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*

extension (type_ : Type)
  /** Substitute a free type variable by a type, renaming binders to avoid capture and preserving
   *  the unsimplified structure of the result. */
  def substitute(var_ : TypeVar, substitute: Type): Type =
    TypeSubstitute1(type_, TypeSubstituteParams(var_, substitute))

extension (constraint: Constraint)
  /** Substitute in both operands of a constraint without capturing free variables. */
  def substitute(var_ : TypeVar, replacement: Type): Constraint =
    TypeSubstitute1(constraint, TypeSubstituteParams(var_, replacement))(using TypeSubstitute1)

/** Parameters of the type substitution operation. */
class TypeSubstituteParams(
  val var_ : TypeVar,
  val substitute: Type,
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
        params.substitute
      case TUniv(var_, body) if params.substitute.hasVar(var_) && body.hasVar(params.var_) =>
        val fresh = new TypeVar(newFreshVarName(), var_.provenance)
        TUniv(fresh, first.apply(body.substitute(var_, TVar(fresh)), params))
      case _ =>
        next.apply(type_, params)

private def TypeSubstitute3 = TypeDispatcher[Id, Id, TypeSubstituteParams](Combinator)

private def Combinator = TypeIdentityCombinator[TypeSubstituteParams]
