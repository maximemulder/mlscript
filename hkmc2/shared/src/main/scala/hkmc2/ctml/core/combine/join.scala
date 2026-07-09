package hkmc2.ctml.core.combine

import hkmc2.ctml.config.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.types.*

/** Get the simplified join of two types. */
def join(left: Type, right: Type)(using ctx: SubContext): Type =
  joinWithDebug(joinImpl)(left, right)

/** Implementation of `join`. */
def joinImpl(left: Type, right: Type)(using ctx: SubContext): Type =
  if checkSubtype(left, right) then
    return right

  if checkSubtype(right, left) then
    return left

  TJointType(JointMode.Union, left, right)

def joinMerge(left: Type, right: Type)(using ctx: SubContext): Option[Type] =
  left match
    case TNeg(left) if checkSubtype(left, right) =>
      return Some(TTop)
    case _ =>

  right match
    case TNeg(right) if checkSubtype(right, left) =>
      return Some(TTop)
    case _ =>

  None

extension (types: List[Type])
  /** Get the simplified join of many types. */
  def joinMany()(using ctx: SubContext): Type =
    types.foldRight(TBot)(join)

  def joinManySeq(ins: SubClauses)(using ctx: SubContext): Type =
    given SubContext = ctx.extend(ins)
    types.foldRight(TBot)(join)
