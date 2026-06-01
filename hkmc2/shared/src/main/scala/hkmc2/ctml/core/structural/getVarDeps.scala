package hkmc2.ctml.core.structural

import scala.collection.mutable.Set as MutSet

import hkmc2.ctml.types.*
import hkmc2.ctml.core.context.bound
import hkmc2.ctml.config.debug

extension (var_ : TypeVar)
  /** Get the dependencies of a type variable. */
  def getDeps(pol: Polarity)(using ctx: Context): VarDeps =
    getTypeDeps(var_.bound(pol.dir), pol, true)

  /** Transitively get all the dependencies of a type variable. */
  def getTransDeps(pol: Polarity)(using ctx: Context): VarDeps =
    getVarTransDeps(var_, pol, Set())

  /** Check whether a variable indirectly appears in its bounds. */
  def isIndirectRecursive(pol: Polarity)(using ctx: Context): Boolean =
    var_.getTransDeps(pol).indirect.contains(var_)

/** The dependencies of some type variable. */
class VarDeps(
  val direct: Set[TypeVar],
  val indirect: Set[TypeVar],
):
  /** Concatenate the dependencies with some other ones. */
  def ++(other: VarDeps): VarDeps =
    VarDeps(
      this.direct ++ other.direct,
      this.indirect ++ other.indirect,
    )

  /** Remove a variable from the dependencies. */
  def -(var_ : TypeVar): VarDeps =
    VarDeps(
      this.direct - var_,
      this.indirect - var_,
    )

  /** Make all dependencies indirect. */
  def toIndirect: VarDeps =
    VarDeps(
      Set(),
      this.direct ++ this.indirect
    )

object VarDeps:
  /** The empty dependencies. */
  def empty: VarDeps =
    VarDeps(Set(), Set())

  /** Make dependencies from a single type variable. */
  def single(var_ : TypeVar, direct: Boolean): VarDeps =
    direct match
      case true =>
        VarDeps(Set(var_), Set())
      case false =>
        VarDeps(Set(), Set(var_))

private def getVarTransDeps(var_ : TypeVar, pol: Polarity, cache: Set[TypeVar])(using ctx: Context): VarDeps =
  if cache.contains(var_) then
    return VarDeps.empty

  var deps = var_.getDeps(pol)
  for direct <- deps.direct do
    deps ++= getVarTransDeps(direct, pol, cache + var_)

  for indirect <- deps.indirect do
    deps ++= getVarTransDeps(indirect, pol, cache + var_).toIndirect

  deps

private def getTypeDeps(type_ : Type, pol: Polarity, direct: Boolean): VarDeps =
  type_ match
    case TBot | TTop | TClass(_) =>
      VarDeps.empty
    case TNeg(body) =>
      getTypeDeps(body, !pol, false)
    case TVar(var_) =>
      VarDeps.single(var_, direct)
    case TTuple(left, right) =>
      getTypeDeps(left, pol, false) ++ getTypeDeps(right, pol, false)
    case TLam(param, ret) =>
      getTypeDeps(param, !pol, false) ++ getTypeDeps(ret, pol, false)
    case TUnion(left, right) if pol == Polarity.Positive =>
      getTypeDeps(left, pol, direct) ++ getTypeDeps(right, pol, direct)
    case TInter(left, right) if pol == Polarity.Negative =>
      getTypeDeps(left, pol, direct) ++ getTypeDeps(right, pol, direct)
    case TUnion(left, right) =>
      getTypeDeps(left, pol, false) ++ getTypeDeps(right, pol, false)
    case TInter(left, right) =>
      getTypeDeps(left, pol, false) ++ getTypeDeps(right, pol, false)
    case TApp(abs, arg) =>
      getTypeDeps(abs, pol, false) ++ getTypeDeps(arg, pol, false)
    case TUniv(var_, body) =>
      getTypeDeps(body, pol, false) - var_
    case TConstrained(body, constraint) =>
      getConstraintDeps(constraint, !pol, false) ++ getTypeDeps(body, pol, false)
    case TConstraining(body, constraint) =>
      getConstraintDeps(constraint, !pol, false) ++ getTypeDeps(body, pol, false)

private def getConstraintDeps(constraint: Constraint, pol: Polarity, direct: Boolean): VarDeps =
  getTypeDeps(constraint.left, !pol, direct) ++ getTypeDeps(constraint.right, pol, direct)
