package hkmc2.ctml.core.debug

/** Debugging information. */
object DebugInfo:
  /** The global debug print function. */
  var output: String => Unit = (message) => print(message)

  /** The current call depth. */
  var currentCallDepth = 0

  /** The maximum call depth. */
  val maxCallDepth = 30

  /** The maximum step count. */
  var currentStepCount = 0

  /** The maximum step count. */
  val maxStepCount = 5000

  /** Reset the CTML debug information. */
  def reset() =
    this.currentCallDepth = 0
    this.currentStepCount = 0

/** Debugging flags. */
object DebugFlags:
  /** Show typing context debug flag. */
  var context = false

  /** Show type inference calls debug flag. */
  var infer = false

  /** Show subtype constraining calls debug flag. */
  var constrain = false

  /** Show subtype checking calls debug flag. */
  var check = false

  /** Show type joining calls debug flag. */
  var join = false

  /** Show type meeting calls debug flag. */
  var meet = false

  /** Show type variable calls debug flag. */
  var var_ = false

  /** Apply a flag to the debugging flags */
  def applyFlag(flag: String): Boolean =
    flag match
      case "" =>
        this.reset()
      case "context" =>
        this.context   = true
      case "infer" =>
        this.infer     = true
      case "constrain" =>
        this.constrain = true
      case "check" =>
        this.check     = true
      case "join" =>
        this.join      = true
      case "meet" =>
        this.meet      = true
      case "var" =>
        this.var_      = true
      case _ =>
        return false

    return true

  def reset() =
    this.infer     = false
    this.constrain = false
    this.check     = false
    this.join      = false
    this.meet      = false
    this.var_      = false
