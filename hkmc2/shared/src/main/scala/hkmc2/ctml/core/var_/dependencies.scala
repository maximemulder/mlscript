package hkmc2.ctml.core.var_

import scala.collection.mutable.ListBuffer

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*
import hkmc2.ctml.utils.OrderedSet as MutSet

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
    def show(dependencies: VarDependencies): String =
      dependencies.var_.name

  TreeShow

extension (var_ : TypeVar)
  /** Get the dependencies of a type variable in a context. */
  def getDependencies()(using ctx: Context): VarDependencies =
    given MutSet[TypeVar] = MutSet()
    var_.getDependenciesImpl()

  /** Implementation of `getDependencies`. */
  private def getDependenciesImpl()(using ctx: Context, cache: MutSet[TypeVar]): VarDependencies =
    // Get the direct dependency type variables.
    val dependencyVars = if !cache.contains(var_) then
      ctx.getVarLowerBound(var_).getVars() ++ ctx.getVarUpperBound(var_).getVars()
    else
      Set.empty

    // Add the current variable to the cache.
    cache.add(var_)

    val dependencies = dependencyVars.map(_.getDependenciesImpl())
    VarDependencies(var_, dependencies)

extension (dependencies: VarDependencies)
  /** Get the list of type variables in the type variable dependency graph sorted such that each
   *  variable appears before its dependent variables. */
  def getSortedVars(): List[TypeVar] =
    val vars = ListBuffer[TypeVar]()
    for dependencies <- dependencies.dependencies do
      vars.appendAllUnique(dependencies.getSortedVars())

    vars.appendUnique(dependencies.var_)
    vars.toList

extension (dependencies: List[VarDependencies])
  /** Get the list of type variables in the type variable dependency graphs sorted such that each
   *  variable appears before its dependent variables. */
  def getSortedVars(): List[TypeVar] =
    val vars = ListBuffer[TypeVar]()
    for dependencies <- dependencies do
      vars.appendAllUnique(dependencies.getSortedVars())

    vars.toList
