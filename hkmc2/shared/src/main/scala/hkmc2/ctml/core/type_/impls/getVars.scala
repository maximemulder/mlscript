package hkmc2.ctml.core.type_.impls

import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*
import hkmc2.ctml.util.given

extension (type_ : Type)
  /** Get the type variables that appear in a type. */
  def getVars2(): Set[TypeVar] =
    GetVars(type_, ())

private object GetVars extends TypeDispatcher[Const[Set[TypeVar]], Const[Set[TypeVar]], Unit](TypeMonoidCombinator(summon[Monoid[Set[TypeVar]]])):
  override def apply(type_ : Type, params: Unit)(using first: TypeApplicator[Const[Set[TypeVar]], Unit]): Set[TypeVar] =
    type_ match
      case TVar(var_) =>
        Set(var_)
      case _ =>
        super.apply(type_, params)

  override def apply(bound: Bound, params: Unit): Set[TypeVar] =
    GetVars(bound.type_, params)
