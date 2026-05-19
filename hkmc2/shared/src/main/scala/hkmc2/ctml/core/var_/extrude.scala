package hkmc2.ctml.core.var_

import scala.collection.mutable.Map as MutMap

import hkmc2.ctml.core.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*
import hkmc2.ctml.config.debug
import hkmc2.ctml.config.output

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
  type_ match
    case TVar(var_) if var_.level < level =>
//      var_.kind match
//        case TypeVarKind.Rigid =>
//          extrudeRigidVar(var_)
//        case TypeVarKind.Flex =>
//          extrudeFreshVar(var_)
      cache.get(var_, pol) match
        case Some(type_) =>
          (type_, Clauses.empty)
        case None =>
          extrudeVar(var_)
    case TBot | TTop | TVar(_) | TClass(_) =>
      (type_, Clauses.empty)
    case TNeg(body) =>
      val (newBody, outs) = extrudeType(body)
      (TNeg(newBody), outs)
    case TTuple(left, right) =>
      val (newLeft,  leftOuts)  = extrudeType(left)
      val (newRight, rightOuts) = extrudeTypeSeq(right, leftOuts)
      (TTuple(newLeft, newRight), rightOuts)
    case TLam(param, ret) =>
      val (newParam, paramOuts) =
        given Polarity = pol.invert
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
      val (newAbs, absOuts)  = extrudeType(abs)
      val (newArg, argOuts) = extrudeTypeSeq(arg, absOuts)
      (TApp(newAbs, newArg), argOuts)
    case TUniv(var_, body) =>
      given Context = ctx.declVar(var_, TypeVarKind.Rigid)
      val (newBody, bodyOuts) = extrudeType(body)
      (TUniv(var_, newBody), bodyOuts)
    case TConstrained(body, constraint) =>
      val (newBound, constraintOuts) = extrudeConstraint(constraint)
      val (newBody,  bodyOuts)       = extrudeTypeSeq(body, constraintOuts)
      (TConstrained(newBody, newBound), bodyOuts)
    case TConstraining(body, constraint) =>
      val (newBound, constraintOuts) = extrudeConstraint(constraint)
      val (newBody,  bodyOuts)       = extrudeTypeSeq(body, constraintOuts)
      (TConstraining(newBody, newBound), bodyOuts)

/** Extrude the type variables of a type variable bound. */
private def extrudeConstraint(constraint: Constraint)(using ctx: Context, level: Int, pol: Polarity, cache: ExtrudeCache): (Constraint, Clauses) =
  val (leftType,  leftOuts)  = extrudeType(constraint.left)
  val (rightType, rightOuts) = extrudeTypeSeq(constraint.right, leftOuts)
  (Constraint(leftType, constraint.dir, rightType), rightOuts)

private def extrudeVar(var_ : TypeVar)(using ctx: Context, level: Int, pol: Polarity, cache: ExtrudeCache): (Type, Clauses) =
  // Create new fresh type variable at the right level.
  val (freshVar, freshCtx) = ctx.declFreshVar(TypeVarKind.Flex, Some(var_), Some(level))
  val freshDecl = freshCtx.clauses(0)
  val freshType = TVar(freshVar)

  // Add the new type variable to the cache.
  cache.addOne((var_, pol), freshType)

  // Add the new fresh variable to the original variable bounds.
  val bound = var_.bound(pol.dir.invert())
  val newBound = hkmc2.ctml.core.combine.combine(bound, freshType, pol.dir.invert())(using freshCtx, SubtypingCache())
  debug(s"COMBINING ${bound} AND ${freshType} INTO ${newBound}")
  val x = Bound(var_, pol.dir.invert(), newBound)

  //
  val (newExtrudedBound, outs) = extrudeTypeSeq(var_.bound(pol.dir), Clauses(List(freshDecl, x)))
  val y = Bound(freshVar, pol.dir, newExtrudedBound)
  (freshType, outs.concat(y.asClauses))

/** Extrude a fresh type variable. */
// private def extrudeFreshVar(var_ : TypeVar)(using ctx: Context, level: Int, pol: Polarity, cache: ExtrudeCache): (Type, Clauses) =
//   cache.get(var_, pol) match
//     case Some(type_) =>
//       (type_, Clauses.empty)
//     case None =>
//       debug(s"LAST CLAUSE ZERO ${ctx.clauses.length}")
//       val (freshVar, freshCtx) = ctx.declFreshVar(TypeVarKind.Flex, Some(var_), Some(level))
//       debug(s"EXTRUDE ${var_} WITH ${freshVar}")
//       val freshType = TVar(freshVar)
//       cache.addOne((var_, pol), freshType)
//       val bound = ctx.getVarBound(var_, pol.dir)
//       val newBound = hkmc2.ctml.core.combine.combine(bound, freshType, pol.dir)(using freshCtx, SubtypingCache())
//       debug(s"LAST CLAUSE ${freshCtx.clauses(0)}")
//       val (newExtrudedBound, outs) = extrudeTypeSeq(newBound, freshCtx.clauses(0).asClauses)
//       (freshType, outs.concat(Bound(freshVar, pol.dir, newExtrudedBound).asClauses))
//
// /** Extrude a rigid type variable. */
// private def extrudeRigidVar(var_ : TypeVar)(using ctx: Context, level: Int, pol: Polarity, cache: ExtrudeCache): (Type, Clauses) =
//   cache.get(var_, pol) match
//     case Some(type_) =>
//       (type_, Clauses.empty)
//     case None =>
//       val (freshVar, freshCtx) = ctx.declFreshVar(TypeVarKind.Flex, Some(var_), Some(level))
//       debug(s"EXTRUDE ${var_} WITH ${freshVar}")
//       val freshType = TVar(freshVar)
//       cache.addOne((var_, pol), freshType)
//       val bound = ctx.getVarBound(var_, pol.dir)
//       // given SubtypingCache = SubtypingCache()
//       // given ConstraintMode = ConstraintMode.Solve
//       // val outs = subtypeDirSeq(freshType, bound, pol.dir, freshCtx.clauses(0).asClauses)
//       val newBound = hkmc2.ctml.core.combine.combine(bound, freshType, pol.dir)(using freshCtx, SubtypingCache())
//       debug(s"LAST CLAUSE ${freshCtx.clauses(0)}")
//       val (newExtrudedBound, outs) = extrudeTypeSeq(newBound, freshCtx.clauses(0).asClauses)
//       (freshType, outs.concat(Bound(freshVar, pol.dir, newExtrudedBound).asClauses))
