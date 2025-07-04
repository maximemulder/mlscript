package hkmc2.ctml.core

import scala.collection.mutable.Set as SetMut

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*

/** A type variable and its dependency graph, that is, the other type variables on which it
 *  depends.
 */
class VarDependencyGraph(val var_ : TypeVar, val dependencies: Set[VarDependencyGraph]):
  override def toString(): String =
    val builder = StringBuilder(var_.name)
    if dependencies != Set.empty then
      builder.append(" ")
      builder.append(dependencies)

    builder.toString()

extension (ctx: Context)
  /** Get the dependency graph of a type variable. */
  def getVarDependencies(var_ : TypeVar): VarDependencyGraph =
    getVarDependenciesImpl(var_)(using SetMut())

  /** Implementation of `getVarDependencies`. */
  private def getVarDependenciesImpl(var_ : TypeVar)(using cache: SetMut[TypeVar]): VarDependencyGraph =
    // Get the direct dependency type variables.
    val dependencyVars = if !cache.contains(var_) then
      ctx.getVarLowerBound(var_).getVars() ++ ctx.getVarUpperBound(var_).getVars()
    else
      Set.empty

    // Add the current variable to the cache.
    cache.add(var_)

    val dependencies = dependencyVars.map(ctx.getVarDependenciesImpl(_))
    VarDependencyGraph(var_, dependencies)
