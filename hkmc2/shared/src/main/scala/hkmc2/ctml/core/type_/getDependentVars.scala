package hkmc2.ctml.core.type_

import scala.collection.mutable.Set as MutSet

import hkmc2.ctml.core.type_.impls.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.clauses.typeVars

extension (ctx: Context)
  /** Get the list of type variables that directly depend on another type variable in the clauses. */
  def getDependentVars(var_ : TypeVar): Set[TypeVar] =
    ctx.getDependentVarsInner(var_)(using MutSet())
      .filter((x) => ctx.clauses.typeVars.exists((y) => x == y))
      .filter(_ != var_)

  /** Get the list of type variables that directly depend on another type variable in the clauses. */
  def getDependentVarsInner(var_ : TypeVar)(using cache: MutSet[(TypeVar, Direction)]): Set[TypeVar] =
    ctx.clauses.reverse.iterator.flatMap(ctx.getDependentVarsInner(var_, _)).toSet

  /** Get the list of type variables that directly depend on another type variable in the clause. */
  def getDependentVarsInner(var_ : TypeVar, clause: Clause)(using cache: MutSet[(TypeVar, Direction)]): Set[TypeVar] =
    clause match
      case Bound(boundVar, dir, boundType) if boundVar != var_ && boundType.containsVar(var_) && !cache.contains((boundVar, dir)) =>
        cache.add((boundVar, dir))
        Set(boundVar) ++ ctx.getDependentVarsInner(boundVar)
      case _ =>
        Set.empty
