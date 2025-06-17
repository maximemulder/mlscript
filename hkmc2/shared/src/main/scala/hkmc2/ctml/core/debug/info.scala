package hkmc2.ctml.core.debug

/** Global debug print function. */
var outputter: String => Unit = (message) => print(message)

object DebugInfo:
  /** Global type inference debug flag. */
  var infer = false

  /** Global subtype constraining debug flag. */
  var constrain = false

  /** Global subtype checking debug flag. */
  var check = false

  /** Global type joining debug flag. */
  var join = false

  /** Global type meeting debug flag. */
  var meet = false

  /** Global type variable debug flag. */
  var var_ = false

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
    this.infer     = false
    this.constrain = false
    this.check     = false
    this.join      = false
    this.meet      = false
    this.var_      = false
