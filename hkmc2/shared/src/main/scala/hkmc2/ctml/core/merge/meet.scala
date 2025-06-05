package hkmc2.ctml.core.merge

import hkmc2.ctml.core.*
import hkmc2.ctml.types.*

/** Get the simplified meet of two types. */
def meet(left: Type, right: Type)(using ctx: Clauses): Type =
  debugMeet(meetImpl)(left, right)

/** Implementation of `meet`. */
def meetImpl(left: Type, right: Type)(using ctx: Clauses): Type =
  if checkSubtype(right, left) then
    return right

  if checkSubtype(left, right) then
    return left

  var fusedMeet = meetDisjoint(left, right)
  if fusedMeet.isDefined then
    return fusedMeet.get

  TInter(left, right)

/** Get the meet of two disjoint types in a non-intersection form if possible. */
def meetDisjoint(left: Type, right: Type)(using ctx: Clauses): Option[Type] =
  if left.is[TConstraining] || right.is[TConstraining] then
    val (leftBase,  leftBounds)  = left.splitConstrainings()
    val (rightBase, rightBounds) = right.splitConstrainings()
    debug(s"YOOOO ${left} AND ${right}")
    val base = meet(leftBase, rightBase)
    val bounds = ctx.meetBounds(leftBounds, rightBounds)
    return Some(attachConstrainingBounds(base, bounds))

  if left.is[TLam] && right.is[TLam] then
    meetDisjointLam(left.as[TLam], right.as[TLam])

  None

def meetDisjointLam(left: TLam, right: TLam)(using ctx: Clauses): Option[Type] =
  if checkEqual(left.param, right.param) then
    var body = meet(left.ret, right.ret)
    return Some(TLam(left.param, body))

  if checkEqual(left.ret, right.ret) then
    var param = join(left.param, right.param)
    return Some(TLam(param, left.ret))

  None

extension (types: List[Type])
  /** Get the simplified meet of many types. */
  def meetMany()(using ctx: Clauses): Type =
    types.foldRight(TTop)(meet)
