package hkmc2.ctml.core.config

import scala.collection.mutable.ListBuffer

import hkmc2.ctml.util.*

def applyDebugArguments(arguments: List[String]): Unit =
  try
    var buffer = ListBuffer(arguments*)
    while buffer.nonEmpty do
      buffer.remove(0) match
        case "" =>
          Debug.reset()
        case "context" =>
          Debug.context = true
        case "infer" =>
          Debug.infer = true
        case "constrain" =>
          Debug.constrain = true
        case "check" =>
          Debug.check = true
        case "join" =>
          Debug.join = true
        case "meet" =>
          Debug.meet = true
        case "var" =>
          Debug.var_ = true
        case "depth" =>
          buffer.popFront match
            case Some(depth) =>
              Debug.depth = Some(depth.toInt)
            case None =>
              throw Exception("missing depth value")
        case argument =>
          throw Exception(s"unknown argument '${argument}'")
  catch
    case error: Exception =>
      Config.output(s"Could not parse debug arguments: ${error.getMessage()}")
