package hkmc2.ctml.core.type_

import hkmc2.ctml.core.var_.*
import hkmc2.ctml.core.abstractions.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

extension (type_ : Type)
  /** Subsitute a type variable by another type variable in the type, without simplifying the resulting type. */
  def substitute(var_ : TypeVar, substitute: TypeVar): Type =
    TypeSubstitute(type_, TypeSubstituteParams(var_, substitute))

/** Parameters of the type substitution operation. */
class TypeSubstituteParams(
  val var_ : TypeVar,
  val substitute: TypeVar,
) extends WithTypeVar[TypeSubstituteParams]:
  def getTypeVar = var_
  def setTypeVar(var_ : TypeVar) = TypeSubstituteParams(var_, substitute)

/** Implementation of the type substitution operation. */
object TypeSubstitute extends TypeVarApplicator(
  new TypeDispatcher[Id, Id, TypeSubstituteParams](TypeIdentityCombinator[TypeSubstituteParams]):
    override def apply(type_ : Type, params : TypeSubstituteParams): Type =
      type_ match
        case TVar(var_) =>
          TVar(this.substitute(var_, params))
        case _ =>
          super.apply(type_, params)

    override def apply(bounds: List[Bound], params: TypeSubstituteParams): List[Bound] =
      bounds.map(bound =>
        val newVar  = this.substitute(bound.var_, params)
        val newType = this.apply(bound.type_, params)
        Bound(newVar, bound.dir, newType)
      )

    def substitute(var_ : TypeVar, params: TypeSubstituteParams): TypeVar =
        if var_ == params.var_ then
          params.substitute
        else
          var_
):
  def apply(univ: TUniv): Type =
    univ
