package hkmc2.ctml.core.subtyping

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Split a type in two if it can be decomposed as an union. */
  def splitUnion(pol: Polarity)(using ctx: Context): Option[(Type, Type)] =
    type_.split(JointMode.Union)(using ctx, pol, Set())

  /** Split a type in two if it can be decomposed as an intersection. */
  def splitInter(pol: Polarity)(using ctx: Context): Option[(Type, Type)] =
    type_.split(JointMode.Inter)(using ctx, pol, Set())

  /** Split a union or intersection in two depending on a type polarity. */
  def splitStructure(mode: JointMode): Option[(Type, Type)] =
    (mode, type_) match
      case (JointMode.Union, TUnion(left, right)) =>
        Some(left, right)
      case (JointMode.Inter, TInter(left, right)) =>
        Some(left, right)
      case _ =>
        None

  /** Split a union or intersection like type in two depending on a type polarity. */
  def split(mode: JointMode)(using ctx: Context, pol: Polarity, cache: Set[TypeVar]): Option[(Type, Type)] =
    type_ match
      case TVar(var_) if var_.isRigid && !cache.contains(var_) =>
        val newCache = cache + var_
        return var_.bound(pol.dir).removeDirectVars(newCache, pol).split(mode)(using ctx, pol, newCache)
      case TNeg(body) =>
        // Push negations into the split.
        return body.split(!mode) match
          case Some(left, right) =>
            Some(TNeg(left), TNeg(right))
          case None =>
            None
      case _ =>

    type_.splitStructure(mode) match
      case Some(left, right) =>
        return Some(left, right)
      case _ =>

    type_.splitStructure(!mode) match
      case Some(left, right) =>
        left.split(mode) match
          case Some(innerLeft, innerRight) =>
            return Some(
              structuralCombine(!mode, innerLeft,  right),
              structuralCombine(!mode, innerRight, right),
            )
          case None =>

        right.split(mode) match
          case Some(innerLeft, innerRight) =>
            return Some(
              structuralCombine(!mode, left, innerLeft),
              structuralCombine(!mode, left, innerRight),
            )
          case None =>
      case None =>

    if mode == JointMode.Inter then
      type_ match
        case TLam(param, ret) =>
          param.split(JointMode.Union) match
            case Some(left, right) =>
              return Some(
                TLam(left,  ret),
                TLam(right, ret),
              )
            case None =>

          ret.split(JointMode.Inter) match
            case Some(left, right) =>
              return Some(
                TLam(param, left),
                TLam(param, right),
              )
            case None =>
        case _ =>

    None
