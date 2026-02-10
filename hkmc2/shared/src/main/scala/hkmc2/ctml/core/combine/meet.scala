package hkmc2.ctml.core.combine

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.config.*
import hkmc2.ctml.core.system.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*

/** Get the simplified meet of two types. */
def meet(left: Type, right: Type)(using ctx: Context, cache: SubtypingCache): Type =
  meetWithDebug(meetImpl)(left, right)

/** Implementation of `meet`. */
def meetImpl(left: Type, right: Type)(using ctx: Context, cache: SubtypingCache): Type =
  if checkSubtype(right, left) then
    return right

  if checkSubtype(left, right) then
    return left

  meetMerge(left, right) match
    case Some(mergedType) =>
      mergedType
    case None =>
      TInter(left, right)

/** Get the meet of two non-subsumed types in a non-intersection shape if there is one. */
def meetMerge(left: Type, right: Type)(using ctx: Context, cache: SubtypingCache): Option[Type] =

  left match
    case TNeg(left) =>
      return Some(right.subtract(left))
    case _ =>

  right match
    case TNeg(right) =>
      return Some(left.subtract(right))
    case _ =>

  // Meet lambda types.

  (left, right) match
    case (left: TLam, right: TLam) =>
      meetLambdas(left, right)
    case _ =>

  // Meet disjoint types.

  if areDisjointConstructors(left, right) then
    return Some(TBot)

  None

/** Get the meet of two lambdas in a non-intersection shape if there is one. */
def meetLambdas(left: TLam, right: TLam)(using ctx: Context, cache: SubtypingCache): Option[Type] =
  if checkEqual(left.param, right.param) then
    var body = meet(left.ret, right.ret)
    return Some(TLam(left.param, body))

  if checkEqual(left.ret, right.ret) then
    var param = join(left.param, right.param)
    return Some(TLam(param, left.ret))

  None

extension (types: List[Type])
  /** Get the simplified meet of many types. */
  def meetMany()(using ctx: Context, cache: SubtypingCache): Type =
    types.foldRight(TTop)(meet)
