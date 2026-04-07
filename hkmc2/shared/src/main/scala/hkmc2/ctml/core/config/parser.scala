package hkmc2.ctml.core.config

import scala.collection.mutable.ListBuffer

import hkmc2.ctml.util.*

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
        case "output" =>
          config.debug.output = true
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

def applyConfigArguments(arguments: List[String]): Unit =
  try
    var buffer = ListBuffer(arguments*)
    while buffer.nonEmpty do
      buffer.remove(0) match
        case "" =>
          ()
        case "merge-constred" =>
          config.mergeMode = MergeMode.Constrained
        case "merge-constring" =>
          config.mergeMode = MergeMode.Constraining
        case "subtype-absurd-constred" =>
          config.subtypeAbsurdConstreds = true
        case "error-absurd-constreds" =>
          config.checkUnsolvableConstreds = true
        case "arbitrary-patterns" =>
          config.arbitraryPatterns = true
        case argument =>
          throw Exception(s"unknown argument '${argument}'")
  catch
    case error: Exception =>
      config.output(s"Could not parse config arguments: ${error.getMessage()}")
