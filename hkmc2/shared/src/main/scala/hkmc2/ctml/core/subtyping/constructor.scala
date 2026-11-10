package hkmc2.ctml.core.subtyping

import hkmc2.ctml.types.*
import hkmc2.ctml.core.*
import hkmc2.ctml.core.context.*

extension (type_ : Type)
  /** Check if this type is a constructor type. */
  def isConstructor(using ctx: SubContext): Boolean =
    type_ match
      case TClass(_) =>
        true
      case TField(_, _) =>
        true
      case TTuple(_, _) =>
        true
      case TLam(_, _) =>
        true
      case _ =>
        false

/** Check if two types are disjoint constructor types: two classes are disjoint unless one inherits
 *  from the other. Fields can coexist with other fields and class tags, but not with tuples or
 *  functions. Class tags, tuples and functions are otherwise disjoint. */
def areDisjointConstructors(left: Type, right: Type)(using ctx: SubContext): Boolean =
  (left, right) match
    case (TField(_, _), TField(_, _) | TClass(_)) | (TClass(_), TField(_, _)) =>
      false
    case (TLam(_, _), TLam(_, _)) =>
      false
    case (TTuple(_, _), TTuple(_, _)) =>
      false
    case (TClass(left), TClass(right)) if left.isSubClass(right) || right.isSubClass(left) =>
      false
    case _ =>
      left.isConstructor && right.isConstructor
