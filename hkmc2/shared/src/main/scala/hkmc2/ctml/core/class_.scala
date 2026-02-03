package hkmc2.ctml.core

import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*

extension (class_ : TypeVar)
  /** Check whether this class is a sub-class of another class. A class is considered sub-class of
   *  itself. */
  def isSubClass(other: TypeVar)(using ctx: Context): Boolean =
    if class_ == other then
      return true

    ctx.getTypeVarKind(class_) match
      case TypeVarKind.Class(Some(parent)) =>
        parent.isSubClass(other)
      case TypeVarKind.Class(None) =>
        false
      case _ =>
        throw TypeError(Some(s"Type variable '${class_}' is not a class."))
