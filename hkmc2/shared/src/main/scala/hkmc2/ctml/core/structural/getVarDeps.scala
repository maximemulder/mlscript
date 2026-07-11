package hkmc2.ctml.core.structural

import hkmc2.ctml.core.context.bound
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Get the polar dependencies occurring in a type. */
  def getDeps(pol: Polarity): VarDeps =
    getTypeDeps(type_, pol, true)

extension (var_ : TypeVar)
  /** Get the dependencies of the effective bound of a type variable at a given polarity. */
  def getDeps(pol: Polarity)(using ctx: SubContext): VarDeps =
    getTypeDeps(var_.bound(pol.dir), pol, true)

  /** Transitively get all the dependencies of a type variable. */
  def getTransDeps(pol: Polarity)(using ctx: SubContext): VarDeps =
    getVarTransDeps(var_, pol, Set())

  /** Check whether a variable indirectly appears in its bounds. */
  def isIndirectRecursive(pol: Polarity)(using ctx: SubContext): Boolean =
    var_.getTransDeps(pol).indirect.exists(_.var_ == var_)

/** A type variable occurrence together with the polarity selecting its effective bound. */
case class PolarVar(var_ : TypeVar, pol: Polarity)

/** The dependencies of some type variable. */
class VarDeps(
  val direct: Set[PolarVar],
  val indirect: Set[PolarVar],
):
  /** Get all direct and indirect dependencies. */
  def all: Set[PolarVar] =
    direct ++ indirect

  /** Concatenate the dependencies with some other ones. */
  def ++(other: VarDeps): VarDeps =
    VarDeps(
      this.direct ++ other.direct,
      this.indirect ++ other.indirect,
    )

  /** Remove a variable from the dependencies. */
  def -(var_ : TypeVar): VarDeps =
    VarDeps(
      this.direct.filterNot(_.var_ == var_),
      this.indirect.filterNot(_.var_ == var_),
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
  def single(var_ : TypeVar, pol: Polarity, direct: Boolean): VarDeps =
    val dep = PolarVar(var_, pol)
    direct match
      case true =>
        VarDeps(Set(dep), Set())
      case false =>
        VarDeps(Set(), Set(dep))

private def getVarTransDeps(var_ : TypeVar, pol: Polarity, cache: Set[PolarVar])(using ctx: SubContext): VarDeps =
  val ref = PolarVar(var_, pol)
  if cache.contains(ref) then
    return VarDeps.empty

  var deps = var_.getDeps(pol)
  for direct <- deps.direct do
    deps ++= getVarTransDeps(direct.var_, direct.pol, cache + ref)

  for indirect <- deps.indirect do
    deps ++= getVarTransDeps(indirect.var_, indirect.pol, cache + ref).toIndirect

  deps

private def getTypeDeps(type_ : Type, pol: Polarity, direct: Boolean): VarDeps =
  type_ match
    case TBot | TTop | TClass(_) =>
      VarDeps.empty
    case TNeg(body) =>
      getTypeDeps(body, !pol, false)
    case TVar(var_) =>
      VarDeps.single(var_, pol, direct)
    case TTuple(left, right) =>
      getTypeDeps(left, pol, false) ++ getTypeDeps(right, pol, false)
    case TLam(param, ret) =>
      getTypeDeps(param, !pol, false) ++ getTypeDeps(ret, pol, false)
    case TJointType(mode, left, right) =>
      val newDirect = direct && mode.isNaturalPol(pol)
      getTypeDeps(left, pol, newDirect) ++ getTypeDeps(right, pol, newDirect)
    case TApp(abs, arg) =>
      getTypeDeps(abs, pol, false) ++ getTypeDeps(arg, pol, false)
    case TUniv(var_, body) =>
      getTypeDeps(body, pol, false) - var_
    case TConstrained(body, constraint) =>
      getConstraintDeps(constraint, false) ++ getTypeDeps(body, pol, false)
    case TConstraining(body, constraint) =>
      getConstraintDeps(constraint, false) ++ getTypeDeps(body, pol, false)

private def getConstraintDeps(constraint: Constraint, direct: Boolean): VarDeps =
  getTypeDeps(constraint.left, constraint.dir.rightPol, direct) ++
    getTypeDeps(constraint.right, constraint.dir.leftPol, direct)
