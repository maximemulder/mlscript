package hkmc2.ctml.core.var_

import scala.collection.mutable.Set as SetMut

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

/** A type variable and its dependency graph, that is, the other type variables on which it
 *  depends.
 */
class VarDependencies(val var_ : TypeVar, val dependencies: Set[VarDependencies]):
  override def toString: String =
    this.show

/** Implementation of the `Tree` trait for `VarDependencies`. */
given Tree[VarDependencies] with
  override def children(value: VarDependencies) =
    value.dependencies.toList

/** Implementation of the `Show` trait for `VarDependencies`. */
given Show[VarDependencies] =
  given Show[VarDependencies] with
    def show(dependencies: VarDependencies) =
      dependencies.var_.name

  TreeShow

extension (ctx: Context)
  /** Get the dependency graph of a type variable. */
  def getVarDependencies(var_ : TypeVar): VarDependencies =
    getVarDependenciesImpl(var_)(using SetMut())

  /** Implementation of `getVarDependencies`. */
  private def getVarDependenciesImpl(var_ : TypeVar)(using cache: SetMut[TypeVar]): VarDependencies =
    // Get the direct dependency type variables.
    val dependencyVars = if !cache.contains(var_) then
      ctx.getVarLowerBound(var_).getVars() ++ ctx.getVarUpperBound(var_).getVars()
    else
      Set.empty

    // Add the current variable to the cache.
    cache.add(var_)

    val dependencies = dependencyVars.map(ctx.getVarDependenciesImpl(_))
    VarDependencies(var_, dependencies)
