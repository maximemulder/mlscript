package hkmc2.ctml.core

import hkmc2.ctml.types.*

var outputter: (String) => Unit = (message) => print(message)

var currentRecursion = 0
val maxRecursion = 20

def debug(value : Any*) =
  outputter(("  " * currentRecursion) + value.map(_.toString()).mkString(" "))

def debugConstrainSub(impl: (Type, Type) => Clauses, sub: Type, sup: Type)(using ctx: Clauses, mode: Mode): Clauses =
  if currentRecursion > maxRecursion then
    throw TypeError(Some("Reached maximum recursion"))

  try
    debug(s"${mode} ${sub} ≤ ${sup}")
    val outs = try
      currentRecursion += 1
      constrainSubImpl(sub, sup)
    finally
      currentRecursion -= 1
    debug(s"OK ${outs}")
    outs
  catch
    case error: TypeError =>
      debug("FAIL")
      throw error

def debugInfer(impl: (Expr, Clauses) => (Type, Clauses), expr: Expr, ctx: Clauses): (Type, Clauses) =
  if currentRecursion > maxRecursion then
    throw TypeError(Some("Reached maximum recursion"))

  try
    debug(s"infer ${expr}")
    val (type_, outs) = try
      currentRecursion += 1
      inferImpl(expr, ctx)
    finally
      currentRecursion -= 1
    debug(s"OK ${type_} ⇝ ${outs}")
    (type_, outs)
  catch
    case error: TypeError =>
      debug("FAIL")
      throw error
