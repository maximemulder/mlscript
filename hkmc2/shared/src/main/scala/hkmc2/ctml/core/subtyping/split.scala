package hkmc2.ctml.core.subtyping

import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*

/** Combine two types into a union or intersection depending on a type polarity. */
def combine(left: Type, right: Type, pol: Polarity) =
  pol match
    case Polarity.Negative =>
      TUnion(left, right)
    case Polarity.Positive =>
      TInter(left, right)

// TODO: This algorithm might be too greedy, in particular, we should not decompose the bounds of
// type variables eagerly, as doing so without checking subtyping might lose some information.
extension (type_ : Type)
  /** Split a type in two if it can be decomposed as an union. */
  def splitUnion(dir: Direction)(using ctx: Context): Option[(Type, Type)] =
    type_.split(Polarity.Negative)(using ctx, dir, Set())

  /** Split a type in two if it can be decomposed as an intersection. */
  def splitInter(dir: Direction)(using ctx: Context): Option[(Type, Type)] =
    type_.split(Polarity.Positive)(using ctx, dir, Set())

extension (type_ : Type)
  /** Split a union or intersection in two depending on a type polarity. */
  def splitStructure(pol: Polarity)(using ctx: Context, dir: Direction, cache: Set[TypeVar]): Option[(Type, Type)] =
      (pol, type_) match
        case (Polarity.Negative, TUnion(left, right)) =>
          Some(left, right)
        case (Polarity.Positive, TInter(left, right)) =>
          Some(left, right)
        case _ =>
          None

  /** Split a union or intersection like type in two depending on a type polarity. */
  def split(pol: Polarity)(using ctx: Context, dir: Direction, cache: Set[TypeVar]): Option[(Type, Type)] =
    type_ match
      case TVar(var_) if var_.isRigid && !cache.contains(var_) =>
        return var_.bound(dir).split(pol)(using ctx, dir, cache + var_)
      case TNeg(body) =>
        return body.split(pol.invert)
      case _ =>

    type_.splitStructure(pol) match
      case Some(left, right) =>
        return Some(left, right)
      case _ =>

    type_.splitStructure(pol.invert) match
      case Some(left, right) =>
        left.splitStructure(pol) match
          case Some(innerLeft, innerRight) =>
            return Some(
              combine(innerLeft,  right, pol.invert),
              combine(innerRight, right, pol.invert),
            )
          case None =>

        right.splitStructure(pol) match
          case Some(innerLeft, innerRight) =>
            return Some(
              combine(left, innerLeft,  pol.invert),
              combine(left, innerRight, pol.invert),
            )
          case None =>
      case None =>

    if pol == Polarity.Positive then
      type_ match
        case TLam(param, ret) =>
          param.splitStructure(Polarity.Negative) match
            case Some(left, right) =>
              return Some(
                TLam(left,  ret),
                TLam(right, ret),
              )
            case None =>

          ret.splitStructure(Polarity.Positive) match
            case Some(left, right) =>
              return Some(
                TLam(param, left),
                TLam(param, right),
              )
            case None =>
        case _ =>
    None
