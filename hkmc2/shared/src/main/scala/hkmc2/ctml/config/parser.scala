package hkmc2.ctml.config

import scala.collection.mutable.ListBuffer

import hkmc2.ctml.utils.*
import hkmc2.ctml.core.subtyping.ProvenancePolicy

def applyConfigArguments(arguments: List[String]): Unit =
  try
    var buffer = ListBuffer(arguments*)
    while buffer.nonEmpty do
      buffer.remove(0) match
        case "" =>
          ()
        case "extrude-var" =>
          config.extrudeVar = true
        case "no-hypothesis" =>
          config.hypothesis = false
        case "ex-falso" =>
          config.exFalso = true
        case "error-absurd-constred" =>
          config.checkUnsolvableConstreds = true
        case "arbitrary-patterns" =>
          config.arbitraryPatterns = true
        case "wrap-ifs" =>
          config.wrapIfs = true
        case "no-wrap-ifs" =>
          config.wrapIfs = false
        case "dewrap-ucs-ifs" =>
          config.dewrapUcsIfs = true
        case "wrap-ucs-ifs" =>
          config.dewrapUcsIfs = false
        case "profile-infer" =>
          config.profileInfer = true
        case "profile-search" =>
          config.profileSearch = true
        case "provenance-observe" =>
          config.provenancePolicy = ProvenancePolicy.Observe
        case "provenance-cut" =>
          config.provenancePolicy = ProvenancePolicy.Cut
        case "no-provenance" =>
          config.provenancePolicy = ProvenancePolicy.Off
        case "no-search-limit" =>
          config.searchSteps = None
        case "no-check-limit" =>
          config.checkSteps = None
        case option @ ("search-steps" | "check-steps") =>
          val value = buffer.popFront.map(_.toInt).filter(_ > 0)
            .getOrElse(throw Exception(s"$option requires a positive integer"))
          if option == "search-steps" then config.searchSteps = Some(value)
          else config.checkSteps = Some(value)
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
