package hkmc2.ctml.core.debug

import hkmc2.ctml.types.*

/** Convert a value to a string and print it with the debug print function. */
def debug(value : Any*) =
  outputter(("  " * DebugInfo.currentCallDepth) + value.map(_.toString()).mkString(" "))

def printDebug(value : Any*) = debug(value*)

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

/** Print a type variable declaration as a debug information. */
def debugTypeVar(decl : TypeVarDecl): TypeVarDecl =
  if !DebugInfo.var_ then
    return decl

  debug(s"${decl.var_} ${decl.kind}")
  decl

/** Decorate the type variable quantification function to print debug information. */
def debugQuantifyVar(impl: (Type, TypeVar, Type, Type) => Type): (Type, TypeVar, Type, Type) => Type =
  if !DebugInfo.var_ then
    return impl

  (type_ : Type, var_ : TypeVar, lowerBound: Type, upperBound: Type) =>
    debug(s"quantify ${var_} with ${lowerBound} and ${upperBound}")
    val newType = impl(type_, var_, lowerBound, upperBound)
    debug(s"= ${newType}")
    newType

/** Decorate the type variable inlining function to print debug information. */
def debugInlineVar(impl: (Type, TypeVar, Type) => Type): (Type, TypeVar, Type) => Type =
  if !DebugInfo.var_ then
    return impl

  (type_ : Type, var_ : TypeVar, bound: Type) =>
    debug(s"inline ${var_} with ${bound}")
    val newType = impl(type_, var_, bound)
    debug(s"= ${newType}")
    newType

/** Decorate the type variable ignoring function to print debug information. */
def debugIgnoreVar(impl: (Type, TypeVar) => Type): (Type, TypeVar) => Type =
  if !DebugInfo.var_ then
    return impl

  (type_ : Type, var_ : TypeVar) =>
    debug(s"ignore ${var_}")
    val newType = impl(type_, var_)
    debug(s"= ${newType}")
    newType

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
