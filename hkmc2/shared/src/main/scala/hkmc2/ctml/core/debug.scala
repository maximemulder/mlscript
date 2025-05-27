package hkmc2.ctml.core

import hkmc2.ctml.types.*

var outputter: (String) => Unit = (message) => print(message)

var tab = 0

def debug(value : Any) =
  outputter(("  " * tab) + value.toString())


def debugConstrainSub(impl: (Type, Type) => Clauses, sub: Type, sup: Type)(using ctx: Clauses, mode: Mode): Clauses =
  try
    val outs = try
      tab += 1
      constrainSubImpl(sub, sup)
    finally
      tab -= 1
    debug(s"${mode} ${sub} ≤ ${sup} OK ${outs}")
    outs
  catch
    case error: TypeError =>
      debug(s"${mode} ${sub} ≤ ${sup} FAIL")
      throw error
