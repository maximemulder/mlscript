package hkmc2.ctml.core.var_

import scala.collection.mutable.Map as MutMap

import hkmc2.ctml.core.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

type ExtrudeCache = MutMap[(TypeVar, Polarity), Type]

// TODO: Problems to solve related to append-only context:
// - cannot declare variables in read-only context (at a lower level)
// - propagate generated clauses
def extrude(type_ : Type, level: TypeVar)(using ctx: Context, pol: Polarity, cache: ExtrudeCache): (Type, Clauses) =
  type_ match
    case TVar(var_) if ctx.compareVarLevels(var_, level) == Order.Greater =>
      ctx.getTypeVarKind(var_) match
        case hkmc2.ctml.types.TypeVarKind.Class =>
          (type_, Clauses.empty)
        case TypeVarKind.Rigid =>
          extrudeRigidVar(var_, level)
        case TypeVarKind.Fresh =>
          extrudeFreshVar(var_, level)
    case TBot | TTop | TVar(_) =>
      (type_, Clauses.empty)
    case TLam(param, ret) =>
      val newParam =
        given Polarity = pol.invert()
        extrude(param, level)
      val newRet = extrude(ret, level)
      ???
    case TUnion(left, right) =>
      ???
    case TInter(left, right) =>
      ???
    case TConstrained(_, base, _) =>
      ???
    case TConstraining(base, _) =>
      ???

def extrudeFreshVar(var_ : TypeVar, level: TypeVar)(using ctx: Context, pol: Polarity, cache: ExtrudeCache): (Type, Clauses) =
  cache.get(var_, pol) match
    case Some(type_) =>
      (type_, Clauses.empty)
    case None =>
      // TODO: Propagate new decl. Create variable at the right level.
      val freshDecl = declNewFreshVar()
      val freshType = TVar(freshDecl.var_)
      cache.addOne((var_, pol), freshType)
      given Context = ctx.extend(freshDecl)
      pol match
        case Polarity.Negative =>
          val lowerBound = ctx.getVarLowerBound(var_)
          val newLowerBound = join(lowerBound, freshType)
          // TODO: This is the new lower bound, not the extruded type.
          extrude(newLowerBound, level)
        case Polarity.Positive =>
          val upperBound = ctx.getVarUpperBound(var_)
          val newUpperBound = meet(upperBound, freshType)
          // TODO: This is the new upper bound, not the extruded type.
          extrude(newUpperBound, level)

def extrudeRigidVar(var_ : TypeVar, level: TypeVar)(using ctx: Context, pol: Polarity, cache: ExtrudeCache): (Type, Clauses) =
  cache.get(var_, pol) match
    case Some(type_) =>
      (type_, Clauses.empty)
    case None =>
      // TODO: Propagate new decl. Create variable at the right level.
      val freshDecl = declNewFreshVar()
      val freshType = TVar(freshDecl.var_)
      cache.addOne((var_, pol), freshType)
      given Context = ctx.extend(freshDecl)
      val clauses = pol match
        case Polarity.Negative =>
          val upperBound = ctx.getVarUpperBound(var_)
          subtype(upperBound, freshType)
        case Polarity.Positive =>
          val lowerBound = ctx.getVarLowerBound(var_)
          subtype(freshType, lowerBound)
      (freshType, clauses)
