package hkmc2.ctml.core

import hkmc2.ctml.types.*

var outputter: (String) => Unit = (message) => print(message)

var tab = 0

def debug(value : Any) =
  outputter(("  " * tab) + value.toString())

def debugType(type_ : Type) =
  outputter(("  " * tab) + type_.show())

def debugBounds(bounds: List[Bound]) =
  outputter(("  " * tab) + bounds.map(_.show()).mkString(", "))
