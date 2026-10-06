package hkmc2.ctml.config

import scala.collection.mutable.ListBuffer

import hkmc2.ctml.utils.*

def applyConfigArguments(arguments: List[String]): Unit =
  try
    var buffer = ListBuffer(arguments*)
    while buffer.nonEmpty do
      buffer.remove(0) match
        case "" =>
          ()
        case "assumption-reconstruct" =>
          config.assumptionMode = AssumptionMode.Reconstruct
        case "assumption-flexify" =>
          config.assumptionMode = AssumptionMode.Flexify
        case "extrude-var" =>
          config.extrudeVar = true
        case "reconstruct-coherence" =>
          config.reconstructCoherence = true
        case "subtype-absurd-constred" =>
          config.subtypeAbsurdConstreds = true
        case "error-absurd-constred" =>
          config.checkUnsolvableConstreds = true
        case "arbitrary-patterns" =>
          config.arbitraryPatterns = true
        case "general-complement" =>
          config.generalComplement = true
        case argument =>
          throw Exception(s"unknown argument '${argument}'")
  catch
    case error: Exception =>
      config.output(s"Could not parse config arguments: ${error.getMessage()}")

def applyDebugArguments(arguments: List[String]): Unit =
  try
    var buffer = ListBuffer(arguments*)
    while buffer.nonEmpty do
      buffer.remove(0) match
        case "" =>
          config.debug.enabled = true
        case "context" =>
          config.debug.context = true
        case "infer" =>
          config.debug.infer = true
        case "constrain" =>
          config.debug.constrain = true
        case "check" =>
          config.debug.check = true
        case "join" =>
          config.debug.join = true
        case "meet" =>
          config.debug.meet = true
        case "var" =>
          config.debug.var_ = true
        case "inline" =>
          config.debug.inline = true
        case "quantify" =>
          config.debug.quantify = true
        case "extrude" =>
          config.debug.extrude = true
        case "output" =>
          config.debug.output = true
        case "trail" =>
          config.debug.trail = true
        case "depth" =>
          buffer.popFront match
            case Some(depth) =>
              config.debug.depth = Some(depth.toInt)
            case None =>
              throw Exception("missing depth value")
        case argument =>
          throw Exception(s"unknown argument '${argument}'")
  catch
    case error: Exception =>
      config.output(s"Could not parse debug arguments: ${error.getMessage()}")
