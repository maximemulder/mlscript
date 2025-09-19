package hkmc2.ctml.core.combine

import hkmc2.ctml.core.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.debug.*
import hkmc2.ctml.types.*

/** Get the simplified join of two types. */
def join(left: Type, right: Type)(using ctx: Context): Type =
  joinWithDebug(joinImpl)(left, right)

/** Implementation of `join`. */
def joinImpl(left: Type, right: Type)(using ctx: Context): Type =
  val a =
    given VarCache = VarCache()
    checkSubtype(left, right)
  if a then
    return right

  val b =
    given VarCache = VarCache()
    checkSubtype(right, left)
  if b then
    return left

  TUnion(left, right)

extension (types: List[Type])
  /** Get the simplified join of many types. */
  def joinMany()(using ctx: Context): Type =
    types.foldRight(TBot)(join)

  def joinManySeq(ins: Clauses)(using ctx: Context): Type =
    given Context = ctx.extend(ins)
    types.foldRight(TBot)(join)
