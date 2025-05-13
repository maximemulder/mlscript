package hkmc2.ctml.core

import hkmc2.ctml.types.*

/** Get the simplified meet of two types. */
def meet(left: Type, right: Type)(using ctx: Context): Type =
  if checkSub(right, left) then
    return left

  if checkSub(left, right) then
    return right

  var fusedMeet = meetDisjoint(left, right)
  if fusedMeet.isDefined then
    return fusedMeet.get

  TInter(left, right)

/** Get the meet of two disjoint types in a non-intersection form if possible. */
def meetDisjoint(left: Type, right: Type)(using ctx: Context): Option[Type] =
  (left, right) match
    case (left: TLam, right: TLam) =>
      if checkEq(left.param, right.param) then
        var body = meet(left.ret, right.ret)
        return Some(TLam(left.param, body))

      if checkEq(left.ret, right.ret) then
        var param = join(left.param, right.param)
        return Some(TLam(param, left.ret))

      None
    case _ =>
      None
