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
