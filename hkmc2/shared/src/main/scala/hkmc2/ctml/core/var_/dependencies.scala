package hkmc2.ctml.core.var_

import scala.collection.mutable.ListBuffer

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*
import hkmc2.ctml.utils.OrderedSet as MutSet

/** A type variable and its dependency graph, that is, the other type variables on which it
 *  depends.
 */
class VarDependencies(val var_ : TypeVar, val pol: Polarity, val dependencies: Set[VarDependencies]):
  override def toString: String =
    this.show

  def toSet: Set[TypeVar] =
    dependencies.map((dependency) => Set(dependency.var_) ++ dependency.toSet).flatten

/** Implementation of the `Tree` trait for `VarDependencies`. */
given Tree[VarDependencies] with
  override def children(value: VarDependencies) =
    value.dependencies.toList

/** Implementation of the `Show` trait for `VarDependencies`. */
given Show[VarDependencies] =
  given Show[VarDependencies] with
    def show(dependencies: VarDependencies): String =
      dependencies.var_.name

  TreeShow

extension (var_ : TypeVar)
  def getDependencies(pol: Polarity)(using ctx: Context): VarDependencies =
    given MutSet[(Polarity, TypeVar)] = MutSet()
    var_.getDependenciesInner(pol)

  def getDependenciesInner(pol: Polarity)(using ctx: Context, cache: MutSet[(Polarity, TypeVar)]): VarDependencies =
    // Get the direct dependency type variables.
    val dependencyVars = if cache.contains((pol, var_)) then
      Set.empty[(Polarity, TypeVar)]
    else
      cache.add((pol, var_))
      pol match
        case Polarity.Negative =>
          var_.upperBound.getPolVars(using Polarity.Negative)
        case Polarity.Positive =>
          var_.lowerBound.getPolVars(using Polarity.Positive)

    val dependencies = dependencyVars.map((pol, var_) => var_.getDependenciesInner(pol))
    VarDependencies(var_, pol, dependencies)
