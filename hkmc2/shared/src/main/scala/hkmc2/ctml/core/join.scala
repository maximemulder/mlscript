package hkmc2.ctml.core

import hkmc2.ctml.types.*

/** Get the simplified join of two types. */
def join(left: Type, right: Type)(using ctx: Context): Type =
  if checkSub(left, right) then
    return left

  if checkSub(right, left) then
    return right

  TUnion(left, right)
