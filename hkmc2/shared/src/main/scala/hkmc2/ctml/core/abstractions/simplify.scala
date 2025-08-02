package hkmc2.ctml.core.abstractions

import hkmc2.ctml.core.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

class TypeSimplifyCombinator[P <: WithContext[P]] extends TypeCombinator[Const[Type], Id, P]:
  def bot(params: P): Type =
    TBot

  def top(params: P): Type =
    TTop

  def var_(var_ : TypeVar): Type =
    TVar(var_)

  def lam(param: Type, ret: Type, params: P): Type =
    TLam(param, ret)

  def union(left: Type, right: Type, params: P): Type =
    join(left, right)(using params.getContext)

  def inter(left: Type, right: Type, params: P): Type =
    meet(left, right)(using params.getContext)

  def univ(var_ : TypeVar, body: Type, params: P): Type =
    TUniv(var_, body)

  def constrained(body: Type, bounds: List[Bound], params: P): Type =
    TConstrained(body, bounds)

  def constraining(body: Type, bounds: List[Bound], params: P): Type =
    body.attachConstrainingBounds(bounds)(using params.getContext)

  def bounds(bounds: List[Bound], params: P): List[Bound] =
    bounds
