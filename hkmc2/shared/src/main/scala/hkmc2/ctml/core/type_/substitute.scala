package hkmc2.ctml.core.type_

import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.abstractions.TypeDispatcher
import hkmc2.ctml.core.abstractions.TypeIdentityCombinator

extension (type_ : Type)
  /** Replace a type variable by an other type variable in the type, without simplifying the resulting type. */
  def substitute(var_ : TypeVar, substitute: TypeVar): Type =
    TypeSubstituter(type_, SubstitutionParams(var_, substitute))

class SubstitutionParams(val var_ : TypeVar, val substitute: TypeVar)

object TypeSubstituter extends TypeDispatcher[[T] =>> T, SubstitutionParams](TypeIdentityCombinator):
  override def apply(type_ : Type, params : SubstitutionParams): Type =
    type_ match
      case TVar(var_) =>
        TVar(this.substitute(var_, params))
      case _ =>
        super.apply(type_, params)

  override def apply(bounds: List[Bound], params: SubstitutionParams): List[Bound] =
    bounds.map(bound =>
      val newVar  = this.substitute(bound.var_, params)
      val newType = this.apply(bound.type_, params)
      Bound(newVar, bound.dir, newType)
    )

  def substitute(var_ : TypeVar, params: SubstitutionParams): TypeVar =
      if var_ == params.var_ then
        params.substitute
      else
        var_

extension (univ: TUniv)
  /** Substitute the quantified variable of a universal type with a new fresh type variable. */
  def freshen(): TUniv =
    val freshDecl = declNewFreshVar()
    val body = univ.body.substitute(univ.var_, freshDecl.var_)
    TUniv(freshDecl.var_, body)
