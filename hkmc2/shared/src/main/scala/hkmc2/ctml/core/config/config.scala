package hkmc2.ctml.core.config

/** The mode used to merge clauses. */
enum MergeMode:
  /** Merge using constraining types. */
  case Constraining
  /** Merge using constrained types. */
  case Constrained

/** Debugging information. */
class Config:
  /** The global debug print function. */
  var output: String => Unit = (message) => print(message)

  /** The mode used to merge clauses. */
  var mergeMode = MergeMode.Constraining

  /** Whether or not non-class match patterns are authorized. */
  var weirdMatch = false

  /** The current call depth. */
  var currentCallDepth = 0

  /** The maximum call depth. */
  val maxCallDepth = 50

  /** The maximum step count. */
  var currentStepCount = 0

  /** The maximum step count. */
  val maxStepCount = 5000

  /** The debugging configuration. */
  var debug = Debug()

/** Debugging flags. */
class Debug:
  /** Show debugging information flag. */
  var enabled = false

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

  /** Show output clauses debug flag. */
  var output = false

  /** Maximum show depth debug flag. */
  var depth: Option[Int] = None

var config = Config()
