package hkmc2.ctml.core.system

import hkmc2.ctml.types.*

/** Split a type in two if it can be decomposed as an union. */
def splitUnion(type_ : Type): Option[(Type, Type)] =
  type_ match
    case union: TUnion =>
      Some((union.left, union.right))
    case inter: TInter =>
      (splitUnion(inter.left), splitUnion(inter.right)) match
        case (Some(left, right), _) =>
          Some(
            TInter(left,  inter.right),
            TInter(right, inter.right)
          )
        case (_, Some(left, right)) =>
          Some(
            TInter(inter.left, left),
            TInter(inter.left, right),
          )
        case _ =>
          None
    case _ =>
      None

/** Split a type in two if it can be decomposed as an intersection. */
def splitInter(type_ : Type): Option[(Type, Type)] =
  type_ match
    case union: TUnion =>
      (splitInter(union.left), splitInter(union.right)) match
        case (Some(left, right), _) =>
          Some(
            TUnion(left,  union.right),
            TUnion(right, union.right)
          )
        case (_, Some(left, right)) =>
          Some(
            TUnion(union.left, left),
            TUnion(union.left, right),
          )
        case _ =>
          None
    case inter: TInter =>
      Some (inter.left, inter.right)
    case lam: TLam =>
      (splitUnion(lam.param), splitInter(lam.ret)) match
        case (Some(left, right), _) =>
          Some(
            TLam(left,  lam.ret),
            TLam(right, lam.ret),
          )
        case (_, Some(left, right)) =>
          Some(
            TLam(lam.param, left),
            TLam(lam.param, right),
          )
        case _ =>
          None
    case _ =>
      None

//def combine(left: Type, right: Type, pol: Polarity) =
//  pol match
//    case Polarity.Negative =>
//      TUnion(left, right)
//    case Polarity.Positive =>
//      TInter(left, right)
//
//extension (type_ : Type)
//  def split(pol: Polarity): Option[(Type, Type)] =
//    (pol, type_) match
//      case (Polarity.Negative, TUnion(left, right)) =>
//        Some(left, right)
//      case (Polarity.Positive, TInter(left, right)) =>
//        Some(left, right)
//      case _ =>
//        None
//
//  def normalSplit(pol: Polarity): Option[(Type, Type)] =
//    type_.split(pol) match
//      case Some(left, right) =>
//        return Some(left, right)
//      case _ =>
//
//    type_.split(pol.invert) match
//      case Some(left, right) =>
//        left.split(pol) match
//          case Some(innerLeft, innerRight) =>
//            return Some(
//              combine(innerLeft,  right, pol.invert),
//              combine(innerRight, right, pol.invert),
//            )
//          case None =>
//
//        right.split(pol) match
//          case Some(innerLeft, innerRight) =>
//            return Some(
//              combine(left, innerLeft,  pol.invert),
//              combine(left, innerRight, pol.invert),
//            )
//          case None =>
//      case None =>
//
//    if pol == Polarity.Positive then
//      type_ match
//        case TLam(param, ret) =>
//          param.split(Polarity.Negative) match
//            case Some(left, right) =>
//              return Some(
//                TLam(left,  ret),
//                TLam(right, ret),
//              )
//            case None =>
//
//          ret.split(Polarity.Positive) match
//            case Some(left, right) =>
//              return Some(
//                TLam(param, left),
//                TLam(param, right),
//              )
//            case None =>
//        case _ =>
//    None
