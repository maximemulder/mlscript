package hkmc2.ctml.core

import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*

/* Poor man's `is` operator because Scala does not have it. */
extension (type_ : Type)
  /** Check whether the type is a specific type. */
  def is[T <: Type](using class_ : reflect.ClassTag[T]): Boolean =
    type_ match
      case _: T => true
      case _ => false

  /** Convert the type to a more specific expected type. */
  def as[T <: Type](using class_ : reflect.ClassTag[T]): T =
    type_ match
      case type_ : T => type_
      case _ => throw Exception("Incorrect type conversion.")

extension (type_ : Type)(using ctx: Context)
  /** Check whether the type is a class type variable. */
  def isClassVar: Boolean =
    type_ match
      case TVar(var_) =>
        ctx.isVarClass(var_)
      case _ =>
        false

  /** Check whether the type is a fresh type reference. */
  def isFreshVar: Boolean =
    type_ match
      case TVar(var_) =>
        ctx.isTypeVarFresh(var_)
      case _ =>
        false
