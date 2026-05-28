package hkmc2.ctml.config

/** The mode used to merge clauses. */
enum MergeMode:
  /** Merge using constrained types. */
  case Constrained
  /** Merge using constraining types. */
  case Constraining

/** The mode used to make subtyping assumption. */
enum AssumptionMode:
  /** Flexify the context type variable. */
  case Flexify
  /** Use subtyping reconstruction. */
  case Reconstruct

/** The mode used to cache subtyping relations. */
enum CacheMode:
  /** Cache only variable bounds. */
  case Var
  /** Cache all subtyping relations. */
  case All

/** Debugging information. */
class Config:
  /** The global debug print function. */
  var output: String => Unit = (message) => print(message)

  /** Whether to use constrained types or constraing types to merge disjunctive clauses. */
  var mergeMode = MergeMode.Constrained

  /** Whether to use flexification or subtyping reconstruction to process subtyping assumptions. */
  var assumptionMode = AssumptionMode.Reconstruct

  /** Whether to cache only type variable bounds or all subtyping relations. */
  var cacheMode = CacheMode.Var

  /** Whether to check constraint coherence during subtyping reconstruction. */
  var reconstructCoherence = false

  /** Whether to allow absurd constrained types in subtyping or not. */
  var subtypeAbsurdConstreds = false

  /** Whether to check for absurd constrained types during type inference or not. */
  var checkUnsolvableConstreds = false

  /** Whether to allow arbitrary (non-class) patterns or not. */
  var arbitraryPatterns = false

  /** The current call depth. */
  var currentCallDepth = 0

  /** The maximum call depth. */
  val maxCallDepth: Option[Int] = Some(5000)

  /** The maximum step count. */
  var currentStepCount = 0

  /** The maximum step count. */
  val maxStepCount: Option[Int] = Some(60)

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

  /** Show subtyping cache add debug flag. */
  var cacheAdd = false

  /** Show subtyping cache check debug flag. */
  var cacheCheck = false

  /** Show subtyping cache hit debug flag. */
  var cacheHit = false

  /** Show subtyping cache miss debug flag. */
  var cacheMiss = false

  /** Maximum show depth debug flag. */
  var depth: Option[Int] = None

var config = Config()
