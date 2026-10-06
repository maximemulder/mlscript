package hkmc2.ctml.config

/** Debugging information. */
class Config:
  /** The global debug print function. */
  var output: String => Unit = (message) => print(message)

  /** Whether to extrude type variable bounds. */
  var extrudeVar = false

  /** Whether to derive the judgments assumed as hypotheses by the hypothesis rule (see `assume`). */
  var hypothesis = true

  /** Whether a constrained type whose constraint is refuted is a supertype of every type (see
   *  `assume`). */
  var exFalso = false

  /** Whether to check for absurd constrained types during type inference or not. */
  var checkUnsolvableConstreds = false

  /** Whether to allow arbitrary (non-class) patterns or not. */
  var arbitraryPatterns = false

  /** Whether to apply the complement rules to every type rather than to the decided types only
   *  (see `admitsComplement`). */
  var generalComplement = false

  /** The current call depth. */
  var currentCallDepth = 0

  /** The maximum call depth. */
  val maxCallDepth: Option[Int] = Some(60)

  /** The maximum step count. */
  var currentStepCount = 0

  /** The maximum step count. */
  val maxStepCount: Option[Int] = Some(5000)

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

  /** Show type variable inlining calls debug flag. */
  var inline = false

  /** Show type variable quantification calls debug flag. */
  var quantify = false

  /** Show type extrusion calls debug flag. */
  var extrude = false

  /** Show output clauses debug flag. */
  var output = false

  /** Show the judgments that repeat a judgment in progress of the subtyping trail debug flag. */
  var trail = false

  /** Maximum show depth debug flag. */
  var depth: Option[Int] = None

private val configLocal =
  ThreadLocal.withInitial(() => Config())

/** The CTML configuration for the current test or compiler thread. */
def config: Config =
  configLocal.get()

/** Reset the CTML configuration for the current test or compiler thread. */
def resetConfig(): Unit =
  configLocal.set(Config())
