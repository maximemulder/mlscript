package hkmc2.ctml.core

import hkmc2.ctml.types.*

/** Get the simplified join of two types. */
def join(left: Type, right: Type)(using ctx: Clauses): Type =
  joinImpl(left, right)

/** Implementation of `join`. */
def joinImpl(left: Type, right: Type)(using ctx: Clauses): Type =
  if checkSub(left, right) then
    return right

  if checkSub(right, left) then
    return left

  TUnion(left, right)

extension (types: List[Type])
  /** Get the simplified join of many types. */
  def joinMany()(using ctx: Clauses): Type =
    types.foldRight(TBot)(join)
