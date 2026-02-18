package hkmc2.ctml.core.subtyping

import hkmc2.ctml.types.*
import hkmc2.ctml.core.*
import hkmc2.ctml.core.context.*

extension (type_ : Type)
  /** Check if this type is a constructor type. */
  def isConstructor(using ctx: Context): Boolean =
    type_ match
      case TVar(_) if type_.isClassVar =>
        true
      case TTuple(_, _) =>
        true
      case TLam(_, _) =>
        true
      case _ =>
        false

/** Check if two types are disjoint constructor types. */
def areDisjointConstructors(left: Type, right: Type)(using ctx: Context): Boolean =
  (left, right) match
    case (TLam(_, _), TLam(_, _)) =>
      false
    case (TTuple(_, _), TTuple(_, _)) =>
      false
    case (TVar(left), TVar(right)) if left.isClass && right.isClass && (left.isSubClass(right) || right.isSubClass(left)) =>
      false
    case _ =>
      left.isConstructor && right.isConstructor
