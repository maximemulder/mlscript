package hkmc2.ctml.core

import hkmc2.ctml.types.*

/** Global debug print function. */
var outputter: String => Unit = (message) => print(message)

/** Global type inference debug flag. */
var inferDebugFlag = false

/** Global subtype constraining debug flag. */
var constrainDebugFlag = false

/** Global subtype checking debug flag. */
var checkDebugFlag = false

/** Global type joining debug flag. */
var joinDebugFlag = false

/** Global type meeting debug flag. */
var meetDebugFlag = false

/** The current call depth. */
var currentCallDepth = 0

/** The maximum call depth. */
val maxCallDepth = 30

/** The maximum step count. */
var currentStepCount = 0

/** The maximum step count. */
val maxStepCount = 5000

/** Reset the CTML debug flags. */
def resetDebugFlags() =
  currentCallDepth = 0
  currentStepCount = 0
  inferDebugFlag = false
  constrainDebugFlag  = false
  checkDebugFlag = false
  joinDebugFlag = false
  meetDebugFlag = false

/** Convert a value to a string and print it with the debug print function. */
def debug(value : Any*) =
  outputter(("  " * currentCallDepth) + value.map(_.toString()).mkString(" "))

/** Decorate the subtype constraining function to print debug information. */
def subtypeWithDebug(impl: (Type, Type) => Clauses)(using ctx: Clauses, mode: Mode): (Type, Type) => Clauses =
  if mode == Mode.Constrain && !constrainDebugFlag then
    return impl

  if mode == Mode.Check && !checkDebugFlag then
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
def inferWithDebug(impl: Expr => (Type, Clauses))(using ctx: Clauses): Expr => (Type, Clauses) =
  if !inferDebugFlag then
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
def joinWithDebug(impl: (Type, Type) => Type)(using ctx: Clauses): (Type, Type) => Type =
  if !joinDebugFlag then
    return impl

  (left: Type, right: Type) =>
    debug(s"join ${left} and ${right}")
    val type_ = debugCall(() => impl(left, right))
    debug(s"= ${type_}")
    type_

/** Decorate the type meet function to print debug information. */
def meetWithDebug(impl: (Type, Type) => Type)(using ctx: Clauses): (Type, Type) => Type =
  if !meetDebugFlag then
    return impl

  (left: Type, right: Type) =>
    debug(s"meet ${left} and ${right}")
    val type_ = debugCall(() => impl(left, right))
    debug(s"= ${type_}")
    type_

/** Register and call a function in the debug environment. */
def debugCall[T](f: () => T): T =
  if currentStepCount >= maxStepCount then
    throw Exception("Exceeded maximum step count.")

  if currentCallDepth >= maxCallDepth then
    throw Exception("Exceeded maximum call depth.")

  currentStepCount += 1
  currentCallDepth += 1

  try
    f()
  finally
    currentCallDepth -= 1
