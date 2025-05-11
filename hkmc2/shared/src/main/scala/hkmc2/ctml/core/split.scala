package hkmc2.ctml.core

import hkmc2.ctml.types.*

/** Split a type in two if it can be decomposed as an union. */
def splitUnion(type_ : Type): Option[(Type, Type)] =
  type_ match
    case union: TUnion =>
      Some((union.left, union.right))
    case inter: TInter =>
      val leftOption = splitUnion(inter.left)
      if leftOption.isDefined then
        val (left, right) = leftOption.get
        return Some((
          TInter(left,  inter.right),
          TInter(right, inter.right)
        ))

      val rightOption = splitUnion(inter.right)
      if rightOption.isDefined then
        val (left, right) = rightOption.get
        return Some((
          TInter(inter.left, left),
          TInter(inter.left, right),
        ))

      None
    case _ =>
      None

/** Split a type in two if it can be decomposed as an intersection. */
def splitInter(type_ : Type): Option[(Type, Type)] =
  type_ match
    case union: TUnion =>
      val leftOption = splitInter(union.left)
      if leftOption.isDefined then
        val (left, right) = leftOption.get
        return Some((
          TUnion(left,  union.right),
          TUnion(right, union.right)
        ))

      val rightOption = splitInter(union.right)
      if rightOption.isDefined then
        val (left, right) = rightOption.get
        return Some((
          TUnion(union.left, left),
          TUnion(union.left, right),
        ))

      None
    case inter: TInter =>
      Some (inter.left, inter.right)
    case lam: TLam =>
      val paramOption = splitUnion(lam.param)
      if paramOption.isDefined then
        val (left, right) = paramOption.get
        return Some((
          TUnion(left,  lam.body),
          TUnion(right, lam.body)
        ))

      val bodyOption = splitInter(lam.body)
      if bodyOption.isDefined then
        val (left, right) = bodyOption.get
        return Some((
          TUnion(lam.param, left),
          TUnion(lam.param, right),
        ))

      None
    case _ =>
      None
