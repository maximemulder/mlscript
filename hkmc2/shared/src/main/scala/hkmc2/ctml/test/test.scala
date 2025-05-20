package hkmc2.ctml.test

import hkmc2.ctml.core.*
import hkmc2.ctml.types.*
import hkmc2.semantics.Term

/** Run a CTML test on an input term. */
def test(term: Term, output: (String) => Unit): Unit =
  // Assign global CTML debug output function.
  outputter = output

  // Try to parse the input term.
  val meta = try
    parseMeta(term)
  catch
    case error: ParseError =>
      output(s"PARSE ERROR: ${error.getMessage()}")
      return

  try
    // Assign the global CTML fresh variable counter.
    freshVarCounter = 0

    // Run the type inference or type constraining test.
    val bounds = meta match
      case MetaExpr(expr) =>
        testExpr(expr, output)
      case MetaType(rel, left, right) =>
        testTypeRel(rel, left, right, output)

    // Print the generated type bounds if there are some.
    if bounds != Nil then
      output(showBounds(bounds))
  catch
    case error: TypeError =>
      output(s"TYPE ERROR: ${error.getMessage()}")

/** Run a CTML type inference test on an expression. */
def testExpr(expr: Expr, output: (String) => Unit): List[Bound] =
  val (type_, bounds) = infer(expr, Context.primitive)
  output(type_.show())
  bounds

/** Run a CTML type relation test on two types. */
def testTypeRel(rel: TypeRel, left: Type, right: Type, output: (String) => Unit): List[Bound] =
  given Context = Context.primitive
  val bounds = rel match
    case TypeRel.Eq =>
      if !checkEq(left, right) then
        throw new TypeError("FAIL")
      Nil
    case TypeRel.Ne =>
      if checkEq(left, right) then
        throw new TypeError("FAIL")
      Nil
    case TypeRel.Sub =>
      constrainSub(left, right)
    case TypeRel.Sup =>
      constrainSub(right, left)
  output("OK")
  bounds
