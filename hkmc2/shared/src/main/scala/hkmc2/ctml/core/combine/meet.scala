package hkmc2.ctml.core.combine

import hkmc2.ctml.core.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.config.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*

/** Get the simplified meet of two types. */
def meet(left: Type, right: Type)(using ctx: Context, cache: VarCache): Type =
  meetWithDebug(meetImpl)(left, right)

/** Implementation of `meet`. */
def meetImpl(left: Type, right: Type)(using ctx: Context, cache: VarCache): Type =
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
def meetMerge(left: Type, right: Type)(using ctx: Context, cache: VarCache): Option[Type] =

  // Meet union-splittable types.

  splitUnion(left) match
    case Some(leftLeft, leftRight) =>
      return Some(join(
        meet(leftLeft,  right),
        meet(leftRight, right),
      ))
    case None =>

  splitUnion(right) match
    case Some(rightLeft, rightRight) =>
      return Some(join(
        meet(left, rightLeft),
        meet(left, rightRight),
      ))
    case None =>

  // Meet constraining types.

  if left.is[TConstraining] || right.is[TConstraining] then
    val (leftBody,  leftBounds)  = left.getConstrainingComponents
    val (rightBody, rightBounds) = right.getConstrainingComponents
    if checkDisjoint(leftBody, rightBody) then
      return Some(TBot)

    val body = meet(leftBody, rightBody)
    val bounds = ctx.meetBounds(leftBounds, rightBounds)
    return Some(makeConstrainingType(body, bounds))

  // Meet lambda types.

  (left, right) match
    case (left: TLam, right: TLam) =>
      meetLambdas(left, right)
    case _ =>

  // Meet disjoint types.

  if checkDisjoint(left, right) then
    return Some(TBot)

  None

/** Get the meet of two lambdas in a non-intersection shape if there is one. */
def meetLambdas(left: TLam, right: TLam)(using ctx: Context, cache: VarCache): Option[Type] =
  if checkEqual(left.param, right.param) then
    var body = meet(left.ret, right.ret)
    return Some(TLam(left.param, body))

  if checkEqual(left.ret, right.ret) then
    var param = join(left.param, right.param)
    return Some(TLam(param, left.ret))

  None

/** Check if two types are disjoint. */
def checkDisjoint(left: Type, right: Type)(using ctx: Context): Boolean =
  return left.isClassVar && right.isClassVar && left != right

extension (types: List[Type])
  /** Get the simplified meet of many types. */
  def meetMany()(using ctx: Context, cache: VarCache): Type =
    types.foldRight(TTop)(meet)
