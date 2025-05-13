package hkmc2.ctml.core

import hkmc2.ctml.types.*

var outputter: (String) => Unit = (message) => print(message)

def debug(message: String) =
  outputter(message)

def debugType(type_ : Type) =
  outputter(type_.show())

def debugBounds(bounds: Bounds) =
  outputter(bounds.map(_.show()).mkString(", "))
