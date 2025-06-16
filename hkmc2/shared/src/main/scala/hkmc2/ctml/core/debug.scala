package hkmc2.ctml.core

import hkmc2.ctml.types.*

/** Global debug print function. */
var outputter: String => Unit = (message) => print(message)

object DebugInfo:
  /** Global type inference debug flag. */
  var infer = false

  /** Global subtype constraining debug flag. */
  var constrain = false

  /** Global subtype checking debug flag. */
  var check = false

  /** Global type joining debug flag. */
  var join = false

  /** Global type meeting debug flag. */
  var meet = false

  /** Global type variable debug flag. */
  var var_ = false

  /** The current call depth. */
  var currentCallDepth = 0

  /** The maximum call depth. */
  val maxCallDepth = 30

  /** The maximum step count. */
  var currentStepCount = 0

  /** The maximum step count. */
  val maxStepCount = 5000

  /** Reset the CTML debug information. */
  def reset() =
    this.currentCallDepth = 0
    this.currentStepCount = 0
    this.infer     = false
    this.constrain = false
    this.check     = false
    this.join      = false
    this.meet      = false
    this.var_      = false

/** Convert a value to a string and print it with the debug print function. */
def debug(value : Any*) =
  outputter(("  " * DebugInfo.currentCallDepth) + value.map(_.toString()).mkString(" "))

/** Decorate the subtype constraining function to print debug information. */
def subtypeWithDebug(impl: (Type, Type) => Clauses)(using ctx: Context, mode: Mode): (Type, Type) => Clauses =
  if mode == Mode.Constrain && !DebugInfo.constrain then
    return impl

  if mode == Mode.Check && !DebugInfo.check then
    return impl

  (sub: Type, sup: Type) =>
    try
      debug(s"${mode} ${sub} ≤ ${sup}")
      val outs = debugCall(() => impl(sub, sup))
      debug(s"OK ⇝ ${outs}")
      outs
    catch
      case error: TypeError =>
        debug("FAIL")
        throw error

/** Decorate the type inference function to print debug information. */
def inferWithDebug(impl: Expr => (Type, Clauses))(using ctx: Context): Expr => (Type, Clauses) =
  if !DebugInfo.infer then
    return impl

  (expr: Expr) =>
    debug(s"infer ${expr}")

    try
      val (type_, outs) = debugCall(() => impl(expr))
      debug(s"OK ${type_} ⇝ ${outs}")
      (type_, outs)
    catch
      case error: TypeError =>
        debug("FAIL")
        throw error

/** Decorate the type join function to print debug information. */
def joinWithDebug(impl: (Type, Type) => Type)(using ctx: Context): (Type, Type) => Type =
  if !DebugInfo.join then
    return impl

  (left: Type, right: Type) =>
    debug(s"join ${left} and ${right}")
    val type_ = debugCall(() => impl(left, right))
    debug(s"= ${type_}")
    type_

/** Decorate the type meet function to print debug information. */
def meetWithDebug(impl: (Type, Type) => Type)(using ctx: Context): (Type, Type) => Type =
  if !DebugInfo.meet then
    return impl

  (left: Type, right: Type) =>
    debug(s"meet ${left} and ${right}")
    val type_ = debugCall(() => impl(left, right))
    debug(s"= ${type_}")
    type_

/** Print a new type variable as a debug information. */
def debugTypeVar(var_ : TypeVar): TypeVar =
  if !DebugInfo.var_ then
    return var_

  debug(s"${var_.name} ${var_.kind}")
  var_

/** Register and call a function in the debug environment. */
def debugCall[T](f: () => T): T =
  if DebugInfo.currentStepCount >= DebugInfo.maxStepCount then
    throw Exception("Exceeded maximum step count.")

  if DebugInfo.currentCallDepth >= DebugInfo.maxCallDepth then
    throw Exception("Exceeded maximum call depth.")

  DebugInfo.currentStepCount += 1
  DebugInfo.currentCallDepth += 1

  try
    f()
  finally
    DebugInfo.currentCallDepth -= 1
