package hkmc2.ctml.core.subtyping

import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.config.debug

/** Type splitting mode. */
enum SplitMode:
  case Union
  case Inter

  /** Get the string representation of the object. */
  override def toString: String =
    this match
      case Union => "union"
      case Inter => "inter"

  /** Invert the type splitting mode. */
  def invert: SplitMode =
    this match
      case Union => Inter
      case Inter => Union

/** Combine two types into a union or intersection depending on a type polarity. */
def combine(mode: SplitMode, left: Type, right: Type) =
  mode match
    case SplitMode.Union =>
      TUnion(left, right)
    case SplitMode.Inter =>
      TInter(left, right)

extension (type_ : Type)
  /** Split a type in two if it can be decomposed as an union. */
  def splitUnion(pol: Polarity)(using ctx: Context): Option[(Type, Type)] =
    type_.split(SplitMode.Union)(using ctx, pol, Set())

  /** Split a type in two if it can be decomposed as an intersection. */
  def splitInter(pol: Polarity)(using ctx: Context): Option[(Type, Type)] =
    type_.split(SplitMode.Inter)(using ctx, pol, Set())

  /** Split a union or intersection in two depending on a type polarity. */
  def splitStructure(mode: SplitMode): Option[(Type, Type)] =
      (mode, type_) match
        case (SplitMode.Union, TUnion(left, right)) =>
          Some(left, right)
        case (SplitMode.Inter, TInter(left, right)) =>
          Some(left, right)
        case _ =>
          None

  /** Split a union or intersection like type in two depending on a type polarity. */
  def split(mode: SplitMode)(using ctx: Context, pol: Polarity, cache: Set[TypeVar]): Option[(Type, Type)] =
    type_ match
      case TVar(var_) if var_.isRigid && !cache.contains(var_) =>
        return var_.bound(pol.dir).split(mode)(using ctx, pol, cache + var_)
      case TNeg(body) =>
        return body.split(mode.invert)
      case _ =>

    type_.splitStructure(mode) match
      case Some(left, right) =>
        return Some(left, right)
      case _ =>

    type_.splitStructure(mode.invert) match
      case Some(left, right) =>
        left.splitStructure(mode) match
          case Some(innerLeft, innerRight) =>
            return Some(
              combine(mode.invert, innerLeft,  right),
              combine(mode.invert, innerRight, right),
            )
          case None =>

        right.splitStructure(mode) match
          case Some(innerLeft, innerRight) =>
            return Some(
              combine(mode.invert, left, innerLeft),
              combine(mode.invert, left, innerRight),
            )
          case None =>
      case None =>

    if mode == SplitMode.Inter then
      type_ match
        case TLam(param, ret) =>
          param.split(SplitMode.Union) match
            case Some(left, right) =>
              return Some(
                TLam(left,  ret),
                TLam(right, ret),
              )
            case None =>

          ret.split(SplitMode.Inter) match
            case Some(left, right) =>
              return Some(
                TLam(param, left),
                TLam(param, right),
              )
            case None =>
        case _ =>

    None
