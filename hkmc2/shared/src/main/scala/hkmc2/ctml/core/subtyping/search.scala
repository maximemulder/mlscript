package hkmc2.ctml.core.subtyping

import scala.collection.mutable
import scala.util.control.NoStackTrace

import hkmc2.ctml.config.*
import hkmc2.ctml.types.*

/** How abstract revisits affect search. Observation has no effect on solver results. */
enum ProvenancePolicy:
  /** Do not compute abstract revisit keys. */
  case Off
  /** Count collisions without changing search. */
  case Observe
  /** Stop a recurring path with an inconclusive result, never with a proof. */
  case Cut

/** Search exhaustion is distinct from a refuted constraint or a completed derivation. A global
 *  exhaustion aborts the operation; a local one can leave another proof alternative available. */
case class SearchInconclusive(reason: String, global: Boolean = false)
  extends Exception(s"Subtyping search inconclusive: $reason") with NoStackTrace

/** A budget shared by nested optional proof queries. It is local to a checking invocation. */
class CheckingBudget:
  /** The number of subtyping calls made within this optional query. */
  var steps = 0

/** Measurements and resource accounting for one operation, never stored globally or in types.
 *  Descendant branches share counters, but their assumptions and revisit trails remain local. */
class SearchSession:
  /** Number of solver entries, including optional queries. */
  var steps = 0
  /** Number of entries in optional checking queries. */
  var checks = 0
  /** Number of exact variable-side repeats. */
  var exactRepeats = 0
  /** Number of abstract variable-side repeats. */
  var abstractRepeats = 0
  /** Number of local cut events, including nested entries into an exhausted optional query. */
  var cuts = 0
  /** Number of fresh quantified or extruded variables. */
  var freshCount = 0
  /** Number of inference variables allocated before any quantifier opening or extrusion. */
  var inferCount = 0
  /** Largest observed number of clauses in the context. */
  var maxClauses = 0
  /** Largest observed goal size, capped at 4096 syntax nodes. */
  var maxGoalNodes = 0
  /** Allocation multiplicities grouped by provenance, rather than concrete identity. */
  private val allocations = mutable.Map.empty[VarProvenance, Int]

  /** Record a new instance without retaining the concrete variable or its bounds. */
  def fresh(provenance: VarProvenance): Unit =
    freshCount += 1
    if config.profileSearch then
      allocations.update(provenance, allocations.getOrElse(provenance, 0) + 1)

  /** Account for a solver entry and enforce enabled resource limits. */
  def enter(sub: Type, sup: Type)(using ctx: SubContext): Unit =
    steps += 1
    if ctx.trail.checking.isDefined then checks += 1
    if config.profileSearch then
      maxClauses = maxClauses.max(ctx.clauses.size)
      var pending = List(sub, sup)
      var nodes = 0
      while pending.nonEmpty && nodes < 4096 do
        val head = pending.head
        pending = head.components ::: pending.tail
        nodes += 1
      maxGoalNodes = maxGoalNodes.max(nodes)
    config.searchSteps.foreach: limit =>
      if steps > limit then
        throw SearchInconclusive(s"operation exceeded $limit solver calls", true)
    ctx.trail.checking.foreach: budget =>
      budget.steps += 1
      config.checkSteps.foreach: limit =>
        if budget.steps > limit then
          cuts += 1
          throw SearchInconclusive(s"optional query exceeded $limit solver calls")

  /** A deterministic diagnostic summary; wall-clock timings are reported separately. */
  def report: String =
    s"@ctml-search calls=$steps checks=$checks fresh=$freshCount infer=$inferCount " +
      s"origins=${allocations.keysIterator.map(_.root).toSet.size} " +
      s"groups=${allocations.size} max-per-group=${allocations.values.maxOption.getOrElse(0)} " +
      s"exact=$exactRepeats abstract=$abstractRepeats cuts=$cuts " +
      s"max-clauses=$maxClauses max-goal-nodes=$maxGoalNodes"

/** Give an operation a local search session when provenance or profiling is enabled. */
def withSearchSession[T](f: SubContext => T)(using ctx: SubContext): T =
  if ctx.trail.search.isDefined ||
      (config.provenancePolicy == ProvenancePolicy.Off && !config.profileSearch &&
        config.searchSteps.isEmpty && config.checkSteps.isEmpty) then f(ctx)
  else
    val session = SearchSession()
    try f(ctx.mapTrail(_.copy(search = Some(session))))
    finally if config.profileSearch then config.output(session.report)
