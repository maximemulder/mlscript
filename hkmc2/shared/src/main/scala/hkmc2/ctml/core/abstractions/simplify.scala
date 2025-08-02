package hkmc2.ctml.core.abstractions

import hkmc2.ctml.core.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

class TypeSimplifyParams(val ctx: Context) extends WithContext[TypeSimplifyParams]:
  def getContext = ctx
  def setContext(ctx: Context) = TypeSimplifyParams(ctx)

object TypeSimplifyCombinator extends TypeCombinator[Const[Type], Id, TypeSimplifyParams]:
  def bot(params: TypeSimplifyParams): Type =
    TBot

  def top(params: TypeSimplifyParams): Type =
    TTop

  def var_(var_ : TypeVar): Type =
    TVar(var_)

  def lam(param: Type, ret: Type, params: TypeSimplifyParams): Type =
    TLam(param, ret)

  def union(left: Type, right: Type, params: TypeSimplifyParams): Type =
    join(left, right)(using params.getContext)

  def inter(left: Type, right: Type, params: TypeSimplifyParams): Type =
    meet(left, right)(using params.getContext)

  def univ(var_ : TypeVar, body: Type, params: TypeSimplifyParams): Type =
    TUniv(var_, body)

  def constrained(body: Type, bounds: List[Bound], params: TypeSimplifyParams): Type =
    TConstrained(body, bounds)

  def constraining(body: Type, bounds: List[Bound], params: TypeSimplifyParams): Type =
    body.attachConstrainingBounds(bounds)(using params.getContext)

  def bounds(bounds: List[Bound], params: TypeSimplifyParams): List[Bound] =
    bounds

object TypeSimplifier extends TypeContextApplicator[Const[Type], TypeSimplifyParams](TypeSimplifyCombinator)
