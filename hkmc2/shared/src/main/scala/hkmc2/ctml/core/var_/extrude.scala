package hkmc2.ctml.core.var_

import scala.collection.mutable.Map as MutMap
import hkmc2.ctml.util.OrderedSet as MutSet

import hkmc2.ctml.core.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

type ExtrudeCache = MutMap[(TypeVar, Polarity), Type]

extension (type_ : Type)
  /** Extrude the type variables of a type such that no type variable is below a given level. */
  def extrude(level: TypeVar)(using ctx: Context): (Type, Clauses) =
    given TypeVar = level
    given Polarity = Polarity.Positive
    given ExtrudeCache = MutMap()
    extrudeType(type_)

/** Sequentially extrude the type variables of a type. */
private def extrudeTypeSeq(type_ : Type, ins: Clauses)(using ctx: Context, level: TypeVar, pol: Polarity, cache: ExtrudeCache): (Type, Clauses) =
  ctx.seq(extrudeType(type_), ins)

/** Extrude the type variables of a type. */
private def extrudeType(type_ : Type)(using ctx: Context, level: TypeVar, pol: Polarity, cache: ExtrudeCache): (Type, Clauses) =
  type_ match
    case TVar(var_) if ctx.compareVarLevels(var_, level) == Order.Greater =>
      ctx.getTypeVarKind(var_) match
        case TypeVarKind.Class =>
          (type_, Clauses.empty)
        case TypeVarKind.Rigid =>
          extrudeRigidVar(var_)
        case TypeVarKind.Flex =>
          extrudeFreshVar(var_)
    case TBot | TTop | TVar(_) =>
      (type_, Clauses.empty)
    case TTuple(left, right) =>
      val (newLeft,  leftOuts)  = extrudeType(left)
      val (newRight, rightOuts) = extrudeTypeSeq(right, leftOuts)
      (TTuple(newLeft, newRight), rightOuts)
    case TLam(param, ret) =>
      val (newParam, paramOuts) =
        given Polarity = pol.invert()
        extrudeType(param)
      val (newRet, retOuts) = extrudeTypeSeq(ret, paramOuts)
      (TLam(newParam, newRet), retOuts)
    case TUnion(left, right) =>
      val (newLeft,  leftOuts)  = extrudeType(left)
      val (newRight, rightOuts) = extrudeTypeSeq(right, leftOuts)
      (TUnion(newLeft, newRight), rightOuts)
    case TInter(left, right) =>
      val (newLeft,  leftOuts)  = extrudeType(left)
      val (newRight, rightOuts) = extrudeTypeSeq(right, leftOuts)
      (TInter(newLeft, newRight), rightOuts)
    case TUniv(var_, body) =>
      given Context = ctx.extend(declRigidVar(var_))
      val (newBody, bodyOuts) = extrudeType(body)
      (TUniv(var_, newBody), bodyOuts)
    case TConstrained(body, bounds) =>
      val (newBounds, boundsOuts) = extrudeBounds(bounds)
      val (newBody,   bodyOuts)   = extrudeTypeSeq(body, boundsOuts)
      (TConstrained(newBody, newBounds), bodyOuts)
    case TConstraining(body, bounds) =>
      val (newBounds, boundsOuts) = extrudeBounds(bounds)
      val (newBody,   bodyOuts)   = extrudeTypeSeq(body, boundsOuts)
      (TConstraining(newBody, newBounds), bodyOuts)

/** Extrude the type variables of a type variable bound. */
private def extrudeBounds(bounds: List[Bound])(using ctx: Context, level: TypeVar, pol: Polarity, cache: ExtrudeCache): (List[Bound], Clauses) =
  bounds.foldRight((Nil, Clauses.empty))((bound, acc) =>
    val (bounds, ins) = acc
    val (boundType, outs) = extrudeTypeSeq(bound.type_, ins)
    (Bound(bound.var_, bound.dir, boundType) :: bounds, outs)
  )

/** Extrude a fresh type variable. */
private def extrudeFreshVar(var_ : TypeVar)(using ctx: Context, level: TypeVar, pol: Polarity, cache: ExtrudeCache): (Type, Clauses) =
  cache.get(var_, pol) match
    case Some(type_) =>
      (type_, Clauses.empty)
    case None =>
      // TODO: Declare the variable at the right level.
      val freshDecl = declFreshFlexVar()
      val freshType = TVar(freshDecl.var_)
      cache.addOne((var_, pol), freshType)
      val bound = ctx.getVarBound(var_, pol.dir)
      val newBound = hkmc2.ctml.core.combine.combine(bound, freshType, pol.dir)(using ctx.extend(freshDecl))
      val (newExtrudedBound, outs) = extrudeTypeSeq(newBound, freshDecl.asClauses)
      (freshType, outs.concat(Bound(freshDecl.var_, pol.dir, newExtrudedBound).asClauses))

/** Extrude a rigid type variable. */
private def extrudeRigidVar(var_ : TypeVar)(using ctx: Context, level: TypeVar, pol: Polarity, cache: ExtrudeCache): (Type, Clauses) =
  cache.get(var_, pol) match
    case Some(type_) =>
      (type_, Clauses.empty)
    case None =>
      // TODO: Declare the variable at the right level.
      val freshDecl = declFreshFlexVar()
      val freshType = TVar(freshDecl.var_)
      cache.addOne((var_, pol), freshType)
      val bound = ctx.getVarBound(var_, pol.dir)
      val outs = subtypeDirSeq(freshType, bound, pol.dir, freshDecl.asClauses)
      (freshType, outs)
