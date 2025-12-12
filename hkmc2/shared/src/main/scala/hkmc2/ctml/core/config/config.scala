package hkmc2.ctml.core.config

/** The mode used to merge clauses. */
enum MergeMode:
  /** Merge using constraining types. */
  case Constraining
  /** Merge using constrained types. */
  case Constrained

/** Debugging information. */
object Config:
  /** The global debug print function. */
  var output: String => Unit = (message) => print(message)

  /** The mode used to merge clauses. */
  var mergeMode = MergeMode.Constraining

  /** The current call depth. */
  var currentCallDepth = 0

  /** The maximum call depth. */
  val maxCallDepth = 50

  /** The maximum step count. */
  var currentStepCount = 0

  /** The maximum step count. */
  val maxStepCount = 5000

  /** Reset the CTML debug information. */
  def reset() =
    this.mergeMode = MergeMode.Constraining
    this.currentCallDepth = 0
    this.currentStepCount = 0

  def apply(flag: String): Boolean =
    flag match
      case "" =>
        this.mergeMode = MergeMode.Constraining
        return true
      case "constraining" =>
        this.mergeMode = MergeMode.Constraining
        return true
      case "constrained" =>
        this.mergeMode = MergeMode.Constrained
        return true
      case _ =>
        return false

/** Debugging flags. */
object Debug:
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

  /** Maximum show depth debug flag. */
  var depth: Option[Int] = None

  def reset() =
    this.context   = false
    this.infer     = false
    this.constrain = false
    this.check     = false
    this.join      = false
    this.meet      = false
    this.var_      = false
