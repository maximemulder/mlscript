package hkmc2.ctml.core.inference

import scala.collection.mutable
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

/** A candidate contract and the rigid payload variables used when validating it. */
private case class SummaryCandidate(contract: Type, payloads: List[TypeVar])

/** Infer a finite structural candidate, then check the original recursive body against it.
 *  Unsupported shapes use ordinary fixed-point inference. Validation failures are type errors,
 *  never evidence that a recursive constraint may be discarded. */
def inferRecursive(rec: ERec)(using ctx: TypeContext): (Type, SubClauses) =
  val candidate = if hkmc2.ctml.config.config.recursiveSummaries then
    annotatedContract(rec.body).map(SummaryCandidate(_, Nil)).orElse(SummaryBuilder(ctx).candidate(rec))
  else None
  candidate match
    case Some(SummaryCandidate(contract, payloads)) =>
      val rigid = payloads.foldLeft(ctx.mapSub(_.mapLevel(_ + 1)))(
        (c, v) => c.mapSub(_.declTypeVar(v, TypeVarKind.Rigid)))
      val checking = rigid.mapSub(_.mapLevel(_ + 1))
        .extendTerm(TermVarDecl(rec.variable, contract))
      val clauses = checkExpr(rec.body, contract)(using checking)
      // Generalization cannot discharge an obligation by choosing a particular payload type.
      // Package only the generated obligations: checking their Unit witness does not require
      // rediscovering the candidate's already selected recursive result witnesses. The checking
      // query rigidifies enclosing variables, so captured obligations cannot be solved by
      // strengthening the caller's context and then discarded.
      val witness = TClass(ClassVar("Unit"))
      val checked = checking.sub.processLevel(checking.sub.level, witness, clauses)
      if !hkmc2.ctml.core.subtyping.checkSubtype(checked, witness)(using rigid.sub) then
        throw TypeError(Some("The recursive summary leaves unsatisfied body obligations."))
      (payloads.foldRight(contract)(TUniv(_, _)), SubClauses.empty)
    case None => inferTopLevel(EApp(EVar(TermVar("fix", None)),
      ELam(Some(LambdaParam(rec.variable, None)), rec.body)))

/** Recover a complete source signature without guessing any missing parameter types. */
private def annotatedContract(expr: Expr): Option[Type] = expr match
  case ELam(Some(LambdaParam(_, Some(input))), body) =>
    annotatedContract(body).map(TLam(input, _))
  case ELam(None, body) => annotatedContract(body).map(TLam(TClass(ClassVar("Unit")), _))
  case EAscr(_, result) => Some(result)
  case _ => None

/** Finite, flow-insensitive shape collection. All its equations are proposals, not type proofs. */
private class SummaryBuilder(ctx: TypeContext):
  /** Abort candidate construction without turning unsupported syntax into a typing error. */
  private class Unsupported extends RuntimeException

  /** One union-find node with at most one arrow/product and finitely many tagged alternatives. */
  private class Slot:
    /** Representative link. */
    var parent: Option[Slot] = None
    /** Constructor alternatives, with payload slots indexed by immutable field names. */
    val tags = mutable.LinkedHashMap.empty[ClassVar, mutable.LinkedHashMap[String, Slot]]
    /** A function shape. */
    var arrow: Option[(Slot, Slot)] = None
    /** A product shape. */
    var tuple: Option[(Slot, Slot)] = None
    /** An opaque, nonrecursive type supplied by the surrounding context. */
    var atom: Option[Type] = None
    /** Find the canonical node. */
    def root: Slot = parent match
      case None => this
      case Some(p) =>
        val r = p.root
        parent = Some(r)
        r

  /** Whether collection encounters a constructor match and a structural recursive cycle. */
  private var hasMatch = false
  /** Payload variables introduced while serializing the finite graph. */
  private val payloads = mutable.LinkedHashMap.empty[Slot, TypeVar]
  /** Variance of payload occurrences in the structural graph, before recursive encoding. */
  private val polarities = mutable.Map.empty[Slot, Set[Boolean]]
  /** Whether serialization closes a positive recursive shape. */
  private var hasCycle = false

  /** Allocate a node for one source position or copied scheme component. */
  private def fresh(): Slot = new Slot

  /** Merge graph nodes with a worklist. Copy each node's shape before processing child
   *  equalities: a child equality may identify a descendant with the current representative. */
  private def unify(a: Slot, b: Slot): Slot =
    var pending = List((a, b))
    while pending.nonEmpty do
      val (left, right) = pending.head
      pending = pending.tail
      val x = left.root
      val y = right.root
      if x != y then
        y.parent = Some(x)
        for (tag, fields) <- y.tags do
          val target = x.tags.getOrElseUpdate(tag, mutable.LinkedHashMap.empty)
          for (name, value) <- fields do
            target.get(name) match
              case Some(old) => pending ::= (old, value)
              case None => target(name) = value
        (x.arrow, y.arrow) match
          case (Some((a, b)), Some((c, d))) => pending = (a, c) :: (b, d) :: pending
          case (None, Some(pair)) => x.arrow = Some(pair)
          case _ =>
        (x.tuple, y.tuple) match
          case (Some((a, b)), Some((c, d))) => pending = (a, c) :: (b, d) :: pending
          case (None, Some(pair)) => x.tuple = Some(pair)
          case _ =>
        (x.atom, y.atom) match
          case (Some(a), Some(b)) if a != b => throw Unsupported()
          case (None, Some(t)) => x.atom = Some(t)
          case _ =>
    a.root

  /** Obtain an arrow shape, sharing argument and result slots across recursive calls. */
  private def arrow(s: Slot): (Slot, Slot) =
    val r = s.root
    r.arrow.getOrElse:
      val pair = (fresh(), fresh())
      r.arrow = Some(pair)
      pair

  /** Flatten both association directions of a structural record intersection. */
  private def intersections(t: Type): List[Type] = t match
    case TJointType(JointMode.Inter, a, b) => intersections(a) ::: intersections(b)
    case _ => List(t)

  /** Copy a simple external signature, freshening quantified payloads at each use. */
  private def fromType(t: Type, vars: Map[TypeVar, Slot] = Map.empty,
    seen: Set[TypeVar] = Set.empty): Slot = t match
    case TUniv(v, TConstrained(TVar(w), Constraint(TVar(x), Direction.Super, layer)))
      if v == w && v == x =>
      val self = fresh()
      unify(self, fromType(layer, vars.updated(v, self), seen))
    case TUniv(v, body) => fromType(body, vars.updated(v, fresh()), seen)
    case TVar(v) if vars.contains(v) => vars(v)
    case TVar(v) if !seen(v) =>
      val lo = ctx.sub.getVarBound(v, Direction.Super)
      val hi = ctx.sub.getVarBound(v, Direction.Sub)
      if lo == hi then fromType(hi, vars, seen + v)
      else
        val s = fresh()
        s.atom = Some(t)
        s
    case TTop => fresh()
    case TClass(tag) =>
      val s = fresh()
      s.tags(tag) = mutable.LinkedHashMap.empty
      s
    case TLam(a, b) =>
      val s = fresh()
      s.arrow = Some((fromType(a, vars, seen), fromType(b, vars, seen)))
      s
    case TTuple(a, b) =>
      val s = fresh()
      s.tuple = Some((fromType(a, vars, seen), fromType(b, vars, seen)))
      s
    case TJointType(JointMode.Union, a, b) => unify(fromType(a, vars, seen), fromType(b, vars, seen))
    case t: TJointType =>
      val parts = intersections(t)
      val tags = parts.collect { case TClass(tag) => tag }
      if tags.size != 1 then throw Unsupported()
      val s = fromType(TClass(tags.head))
      for p <- parts do p match
        case TClass(_) =>
        case TField(name, value) => s.tags(tags.head)(name) = fromType(value, vars, seen)
        case _ => throw Unsupported()
      s
    case _ => throw Unsupported()

  /** Add constructor alternatives tested at a source match, ignoring history between calls. */
  private def pattern(s: Slot, t: Type): Unit = t match
    case TTop =>
    case TNeg(_) =>
    case _ =>
      unify(s, fromType(t))
      hasMatch = true

  /** Whether a pattern chain ends in an unconditional default rather than an exhaustive test. */
  private def hasDefault(m: EMatch): Boolean = if m.pattern == TTop then true else m.else_ match
    case None => false
    case Some(next: EMatch) => hasDefault(next)
    case Some(_) => true

  /** Collect equations without recursively visiting the body at a recursive call. */
  private def collect(e: Expr, env: Map[TermVar, Slot]): Slot = e match
    case ERec(variable, body) =>
      val self = fresh()
      unify(self, collect(body, env.updated(variable, self)))
    case EVar(v) => env.getOrElse(v, fromType(ctx.getVarType(v)))
    case EUnit => fromType(TClass(ClassVar("Unit")))
    case ELam(param, body) =>
      val input = param.flatMap(_.annotation).fold(fresh())(fromType(_))
      val output = collect(body, param.fold(env)(p => env.updated(p.variable, input)))
      val s = fresh()
      s.arrow = Some((input, output))
      s
    case EApp(fn, arg) =>
      val f = collect(fn, env)
      val a = collect(arg, env)
      val (input, output) = arrow(f)
      unify(input, a)
      output
    case EAscr(e, t) => unify(collect(e, env), fromType(t))
    case m @ EMatch(e, p, yes, no) =>
      val input = collect(e, env).root
      val branchEnv = if hasDefault(m) && p != TTop && !p.isInstanceOf[TNeg] then
        // A test with a default does not restrict the caller's domain. Collect its positive
        // branch with a separate shape; validation retains the actual path intersection.
        val refined = fromType(p)
        env.map((v, s) => v -> (if s.root == input then refined else s))
      else
        pattern(input, p)
        env
      val result = collect(yes, branchEnv)
      no.foreach(e => unify(result, collect(e, env)))
      result
    case EProj(e, name) =>
      val record = collect(e, env).root
      val fields = record.tags.valuesIterator.flatMap(_.get(name)).toList
      if fields.isEmpty then throw Unsupported()
      fields.tail.foldLeft(fields.head)(unify)
    case ETuple(a, b) =>
      val s = fresh()
      s.tuple = Some((collect(a, env), collect(b, env)))
      s
    case ETupleProj(e, first) =>
      val s = collect(e, env).root
      val pair = s.tuple.getOrElse:
        val p = (fresh(), fresh())
        s.tuple = Some(p)
        p
      if first then pair._1 else pair._2
    case ERecord(tag, fields) =>
      val s = fresh()
      s.tags(tag) = mutable.LinkedHashMap.from(fields.map((n, e) => n -> collect(e, env)))
      s
    case _ => throw Unsupported()

  /** Serialize positive record cycles with the existing constrained least-fixed-point encoding.
   *  Cycles crossing arrows/products or requiring mutually dependent binders are unsupported. */
  private def emit(s: Slot, active: Map[Slot, TypeVar] = Map.empty,
    path: Set[Slot] = Set.empty, positive: Boolean = true): Type =
    val r = s.root
    active.get(r) match
      case Some(v) =>
        hasCycle = true
        return TVar(v)
      case _ =>
    if path(r) then throw Unsupported()
    val next = path + r
    if r.atom.isDefined then
      if r.tags.nonEmpty || r.arrow.nonEmpty || r.tuple.nonEmpty then throw Unsupported()
      r.atom.get
    else if r.arrow.isDefined then
      if r.tags.nonEmpty || r.tuple.nonEmpty then throw Unsupported()
      val (a, b) = r.arrow.get
      TLam(emit(a, Map.empty, next, !positive), emit(b, Map.empty, next, positive))
    else if r.tuple.isDefined then
      if r.tags.nonEmpty then throw Unsupported()
      val (a, b) = r.tuple.get
      TTuple(emit(a, Map.empty, next, positive), emit(b, Map.empty, next, positive))
    else if r.tags.nonEmpty then
      val binder = newFreshVar()
      val inner = active.updated(r, binder)
      val alternatives = r.tags.toList.map: (tag, fields) =>
        fields.toList.foldLeft(TClass(tag): Type):
          case (t, (name, value)) =>
            TJointType(JointMode.Inter, t, TField(name, emit(value, inner, next, positive)))
      val layer = alternatives.reduceLeft[Type]((a, b) => TJointType(JointMode.Union, a, b))
      // Only self-recursive layers are encoded; nested cycles must not capture an outer binder.
      import hkmc2.ctml.core.structural.*
      if layer.hasVar(binder) then
        if active.values.exists(layer.hasVar(_)) then throw Unsupported()
        TUniv(binder, TConstrained(TVar(binder), Constraint(TVar(binder), Direction.Super, layer)))
      else layer
    else
      polarities(r) = polarities.getOrElse(r, Set.empty) + positive
      TVar(payloads.getOrElseUpdate(r, newFreshVar()))

  /** Construct a candidate only for matched recursive data, leaving other inference unchanged. */
  def candidate(rec: ERec): Option[SummaryCandidate] =
    try
      val self = fresh()
      unify(self, collect(rec.body, Map(rec.variable -> self)))
      if !hasMatch then return None
      val raw = emit(self)
      import hkmc2.ctml.core.type_.impls.substitute.*
      val unusedInputs = payloads.toList.filter((s, _) => polarities(s) == Set(false))
      val contract = unusedInputs.foldLeft(raw)((t, entry) => t.substitute(entry._2, TTop))
      val retained = payloads.toList.filterNot((s, _) => polarities(s) == Set(false)).map(_._2)
      if !hasCycle then None else Some(SummaryCandidate(contract, retained))
    catch
      case _: Unsupported => None
