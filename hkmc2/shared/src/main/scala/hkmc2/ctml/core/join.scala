package hkmc2.ctml.core

import hkmc2.ctml.types.*

/** Get the simplified join of two types. */
def join(left: Type, right: Type)(using ctx: Context): Type =
  joinImpl(left, right)

/** Implementation of `join`. */
def joinImpl(left: Type, right: Type)(using ctx: Context): Type =
  if checkSub(left, right) then
    return right

  if checkSub(right, left) then
    return left

  TUnion(left, right)

/** Get the simplified join of many types. */
def joinMany(types: List[Type])(using ctx: Context): Type =
  types.foldRight(TTop)(join)
