package hkmc2.ctml.core.var_

import scala.collection.mutable.Map as MutMap

import hkmc2.ctml.core.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

// TODO: Add polarity to the cache.
def extrude(type_ : Type, level: TypeVar)(using ctx: Context, pol: Polarity, cache: MutMap[TypeVar, Type]): Type =
  type_ match
    case TVar(var_) if ctx.compareVarLevels(var_, level) == Order.Greater =>
      ctx.getTypeVarKind(var_) match
        case hkmc2.ctml.types.TypeVarKind.Class =>
          type_
        case TypeVarKind.Rigid =>
          cache.getOrElse(var_, {
            val freshDecl = declNewFreshVar()
            val freshType = TVar(freshDecl.var_)
            cache.addOne(var_, freshType)
            pol match
              case Polarity.Negative =>
                // TODO: Extend context and propagate clauses.
                val upperBound = ctx.getVarUpperBound(var_)
                subtype(upperBound, freshType)
              case Polarity.Positive =>
                // TODO: Extend context and propagate clauses.
                val lowerBound = ctx.getVarLowerBound(var_)
                subtype(freshType, lowerBound)
            freshType
          })
        case TypeVarKind.Fresh =>
          cache.getOrElse(var_, {
            val freshDecl = declNewFreshVar()
            val freshType = TVar(freshDecl.var_)
            cache.addOne(var_, freshType)
            pol match
              case Polarity.Negative =>
                // TODO: Extend context and propagate clauses.
                val lowerBound = ctx.getVarLowerBound(var_)
                val newUpperBound = join(lowerBound, freshType)
                extrude(ctx.getVarUpperBound(freshDecl.var_), level)
              case Polarity.Positive =>
                // TODO: Extend context and propagate clauses.
                val upperBound = ctx.getVarUpperBound(var_)
                val newUpperBound = meet(upperBound, freshType)
                extrude(ctx.getVarLowerBound(freshDecl.var_), level)
            freshType
          })
    case _ =>
      type_.map(extrude(_, level))
