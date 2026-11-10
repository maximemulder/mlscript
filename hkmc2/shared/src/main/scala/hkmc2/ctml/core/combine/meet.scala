package hkmc2.ctml.core.combine

import hkmc2.ctml.config.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*

/** Get the simplified meet of two types. */
def meet(left: Type, right: Type)(using ctx: SubContext): Type =
  meetWithDebug(meetImpl)(left, right)

/** Implementation of `meet`. */
def meetImpl(left: Type, right: Type)(using ctx: SubContext): Type =
  if checkSubtype(right, left) then
    return right

  if checkSubtype(left, right) then
    return left

  meetMerge(left, right) match
    case Some(mergedType) =>
      mergedType
    case None =>
      TJointType(JointMode.Inter, left, right)

/** Get the meet of two non-subsumed types in a non-intersection shape if there is one. */
def meetMerge(left: Type, right: Type)(using ctx: SubContext): Option[Type] =

  left match
    case TNeg(left) =>
      return right.subtractStep(left)
    case _ =>

  right match
    case TNeg(right) =>
      return left.subtractStep(right)
    case _ =>

  mergeFieldRequirements(left, right) match
    case Some(merged) => return Some(merged)
    case None =>

  // Meet lambda types.

  (left, right) match
    case (left: TLam, right: TLam) =>
      return meetLambdas(left, right)
    case _ =>

  // Meet disjoint types.

  if areDisjointConstructors(left, right) then
    return Some(TBot)

  None

/** Get the meet of two lambdas in a non-intersection shape if there is one. */
def meetLambdas(left: TLam, right: TLam)(using ctx: SubContext): Option[Type] =
  if checkEqual(left.param, right.param) then
    var body = meet(left.ret, right.ret)
    return Some(TLam(left.param, body))

  if checkEqual(left.ret, right.ret) then
    var param = join(left.param, right.param)
    return Some(TLam(param, left.ret))

  None

extension (types: List[Type])
  /** Get the simplified meet of many types. */
  def meetMany()(using ctx: SubContext): Type =
    types.foldRight(TTop)(meet)

/** Combine requirements for the same field even when other intersection operands separate them.
 *  A negative requirement refines its payload only if a positive requirement guarantees presence:
 *  `{l: A} ∧ ¬{l: B} = {l: A ∧ ¬B}`, whereas `¬{l: B}` alone also permits an absent field. */
private def mergeFieldRequirements(left: Type, right: Type)(using SubContext): Option[Type] =
  /** Flatten only intersections, preserving the order of their operands. */
  def operands(type_ : Type): List[Type] = type_ match
    case TJointType(JointMode.Inter, left, right) => operands(left) ++ operands(right)
    case _ => List(type_)
  val parts = operands(left) ++ operands(right)
  val fields = parts.collect { case TField(name, _) => name }
  val repeated = fields.find: name =>
    parts.count:
      case TField(`name`, _) | TNeg(TField(`name`, _)) => true
      case _ => false
    > 1
  repeated.map: name =>
    val requirements = parts.collect:
      case TField(`name`, value) => value
      case TNeg(TField(`name`, value)) => TNeg(value)
    val value = requirements.reduce(meet)
    if value == TBot then TBot
    else
      val rest = parts.filterNot:
        case TField(`name`, _) | TNeg(TField(`name`, _)) => true
        case _ => false
      rest.foldLeft(TField(name, value): Type)(meet)
