package hkmc2.ctml.core.var_

import scala.collection.mutable.Map as MutMap

import hkmc2.ctml.config.*
import hkmc2.ctml.core.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*

type ExtrudeCache = MutMap[(TypeVar, Polarity), Type]

extension (type_ : Type)
  /** Extrude the type variables of a type such that no type variable is below a given level. */
  def extrude(level: Int, pol: Polarity)(using ctx: Context): (Type, Clauses) =
    given ExtrudeCache = MutMap()
    extrudeType(type_)(using ctx, level, pol, MutMap())

/** Sequentially extrude the type variables of a type. */
private def extrudeTypeSeq(type_ : Type, ins: Clauses)(using ctx: Context, level: Int, pol: Polarity, cache: ExtrudeCache): (Type, Clauses) =
  ctx.seq(extrudeType(type_), ins)

/** Extrude the type variables of a type. */
private def extrudeType(type_ : Type)(using ctx: Context, level: Int, pol: Polarity, cache: ExtrudeCache): (Type, Clauses) =
  extrudeWithDebug(extrudeTypeImpl)(type_)

/** Extrude the type variables of a type. */
private def extrudeTypeImpl(type_ : Type)(using ctx: Context, level: Int, pol: Polarity, cache: ExtrudeCache): (Type, Clauses) =
  type_ match
    case TVar(var_) if var_.level > level =>
      cache.get(var_, pol) match
        case Some(type_) =>
          (type_, Clauses.empty)
        case None =>
          extrudeVar(var_)
    case TBot | TTop | TVar(_) | TClass(_) =>
      (type_, Clauses.empty)
    case TNeg(body) =>
      given Polarity = !pol
      val (newBody, outs) = extrudeType(body)
      (TNeg(newBody), outs)
    case TTuple(left, right) =>
      val (newLeft,  leftOuts)  = extrudeType(left)
      val (newRight, rightOuts) = extrudeTypeSeq(right, leftOuts)
      (TTuple(newLeft, newRight), rightOuts)
    case TLam(param, ret) =>
      val (newParam, paramOuts) =
        given Polarity = !pol
        extrudeType(param)
      given Polarity = pol
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
    case TApp(abs, arg) =>
      val (newAbs, absOuts) = extrudeType(abs)
      val (newArg, argOuts) = extrudeTypeSeq(arg, absOuts)
      (TApp(newAbs, newArg), argOuts)
    case TUniv(var_, body) =>
      given Context = ctx.declTypeVar(var_, TypeVarKind.Rigid)
      // FIXME: This might not work with shadowing.
      cache.addOne((var_, Polarity.Positive), TVar(var_))
      cache.addOne((var_, Polarity.Negative), TVar(var_))
      val (newBody, bodyOuts) = extrudeType(body)
      cache.remove((var_, Polarity.Positive))
      cache.remove((var_, Polarity.Negative))
      (TUniv(var_, newBody), bodyOuts)
    case TConstrained(body, constraint) =>
      val (newBody, bodyOuts) = extrudeType(body)
      (TConstrained(newBody, constraint), bodyOuts)
    case TConstraining(body, constraint) =>
      val (newBody, bodyOuts) = extrudeType(body)
      (TConstraining(newBody, constraint), bodyOuts)

/** Extrude the type variables of a type variable bound. */
private def extrudeConstraint(constraint: Constraint)(using ctx: Context, level: Int, pol: Polarity, cache: ExtrudeCache): (Constraint, Clauses) =
  val (leftType,  leftOuts)  = extrudeType(constraint.left)(using ctx, level, constraint.dir.leftPol * pol, cache)
  val (rightType, rightOuts) = extrudeTypeSeq(constraint.right, leftOuts)(using ctx, level, constraint.dir.rightPol * pol, cache)
  (Constraint(leftType, constraint.dir, rightType), rightOuts)

private def extrudeVar(var_ : TypeVar)(using ctx: Context, level: Int, pol: Polarity, cache: ExtrudeCache): (Type, Clauses) =
  // Create new fresh type variable at the right level.
  val freshDecl = ctx.declExtrudeVar(var_, level)
  val freshVar  = freshDecl.var_
  val freshType = TVar(freshVar)

  // Add the new type variable to the cache.
  cache.addOne((var_, pol), freshType)

  // Add the new fresh variable to the original variable bounds.
  val bound = var_.bound(pol.dir)
  val newBound = hkmc2.ctml.core.combine.combine(bound, freshType, pol.dir)(using ctx.extend(freshDecl))
  val x = Bound(var_, pol.dir, newBound)

  val (newExtrudedBound, outs) = extrudeTypeSeq(var_.bound(!pol.dir), Clauses(List(freshDecl)).concat(Clauses(removeImplicitBounds(List(x)))))
  val y = Bound(freshVar, !pol.dir, newExtrudedBound)
  (freshType, outs.concat(Clauses(removeImplicitBounds(List(y)))))
