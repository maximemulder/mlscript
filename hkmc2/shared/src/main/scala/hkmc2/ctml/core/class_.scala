package hkmc2.ctml.core

import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*

extension (class_ : ClassVar)
  /** Check whether this class is a sub-class of another class. A class is considered sub-class of
   *  itself. */
  def isSubClass(other: ClassVar)(using ctx: Context): Boolean =
    if class_ == other then
      return true

    ctx.getClassDef(class_).parent match
      case Some(parent) =>
        parent.isSubClass(other)
      case None =>
        false
