package hkmc2.ctml.core

import hkmc2.ctml.types.*

/** Global debug print function. */
var outputter: (String) => Unit = (message) => print(message)

/** The current call depth. */
var currentCallDepth = 0

/** The maximum call depth. */
val maxCallDepth = 30

/** Convert a value to a string and print it with the debug print function. */
def debug(value : Any*) =
  outputter(("  " * currentCallDepth) + value.map(_.toString()).mkString(" "))

/** Decorate the subtype constraining function to print debug information. */
def debugSubtype(impl: (Type, Type) => Clauses)(using ctx: Clauses, mode: Mode): (Type, Type) => Clauses =
  if mode == Mode.Check then
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
def debugInfer(impl: (Expr, Clauses) => (Type, Clauses)): (Expr, Clauses) => (Type, Clauses) =
  (expr: Expr, ctx: Clauses) =>
    debug(s"infer ${expr}")

    try
      val (type_, outs) = debugCall(() => impl(expr, ctx))
      debug(s"OK ${type_} ⇝ ${outs}")
      (type_, outs)
    catch
      case error: TypeError =>
        debug("FAIL")
        throw error

/** Decorate the type join function to print debug information. */
def debugJoin(impl: (Type, Type) => Type)(using ctx: Clauses): (Type, Type) => Type =
    (left: Type, right: Type) =>
    debug(s"join ${left} and ${right}")
    val type_ = debugCall(() => impl(left, right))
    debug(s"= ${type_}")
    type_

/** Decorate the type meet function to print debug information. */
def debugMeet(impl: (Type, Type) => Type)(using ctx: Clauses): (Type, Type) => Type =
  (left: Type, right: Type) =>
    debug(s"meet ${left} and ${right}")
    val type_ = debugCall(() => impl(left, right))
    debug(s"= ${type_}")
    type_

/** Register and call a function in the debug environment. */
def debugCall[T](f: () => T): T =
  if currentCallDepth >= maxCallDepth then
    throw Exception("Exceeded maximum call depth.")

  currentCallDepth += 1

  try
    f()
  finally
    currentCallDepth -= 1
