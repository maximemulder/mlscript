package hkmc2.ctml.core

import hkmc2.ctml.types.*

extension (ctx: Context)
  /** Evaluate a function within a context with a new fresh type variable. */
  def withLevel(f: Context => (Type, List[ContextEntry])): (Type, List[ContextEntry]) =
    val (type_ , entries) = f(ctx)
    val typeCtx = ctx.addEntries(entries)
    val varName ="TODO"
    val polarities =
      given Polarity = Polarity.Positive
      getTypePolarities(type_, varName)
    val (newBounds, (lowerBound, upperBound)) = typeCtx.extractVarBounds(varName)
    val newType = polarities match
      case Polarities(true, true) =>
        val bounds = List(Bound(varName, Direction.Super, lowerBound), Bound(varName, Direction.Sub, upperBound))
        TConstrained(List(varName), type_, bounds)
      case Polarities(true, false) =>
        given Context = typeCtx.addEntries(newBounds)
        substitute(type_, varName, upperBound)
      case Polarities(false, true) =>
        given Context = typeCtx.addEntries(newBounds)
        substitute(type_, varName, lowerBound)
      case Polarities(false, false) =>
        type_
    (newType, entries)

/** Infer the type of an expression. */
def infer(expr: Expr, ctx: Context): (Type, List[ContextEntry]) =
  expr match
    // Variable
    case var_ : EVar =>
      (ctx.getVarType(var_.name), Nil)

    // Lambda abstraction
    case lam: ELam =>
      val freshVar = newFreshVar()
      val var_ = TVar(freshVar.name)
      val paramCtx = ctx.addEntry(freshVar, TermVar(lam.paramName, var_))
      val (bodyType, bodyBounds) = infer(lam.body, paramCtx)
      (TLam(var_, bodyType), bodyBounds)

    // Lambda application
    case app: EApp =>
      val freshVar = newFreshVar()
      val freshCtx = ctx.addEntry(freshVar)
      val (lamType, lamBounds) = infer(app.lam, freshCtx)
      val (argType, argBounds) = infer(app.arg, freshCtx)
      val mockLamType = TLam(argType, TVar(freshVar.name))

      val inferBounds =
        given Context = freshCtx.addEntries(lamBounds, argBounds)
        constrainSub(lamType, mockLamType)
      (TVar(freshVar.name), inferBounds ::: argBounds ::: lamBounds)

    // Type ascription
    case ascr: EAscr =>
      val (inferType, inferBounds) = infer(ascr.expr, ctx)
      val constrainBounds =
        given Context = ctx.addEntries(inferBounds)
        constrainSub(inferType, ascr.type_)
      (ascr.type_, constrainBounds ::: inferBounds)

    case match_ : EMatch =>
      inferMatch(match_, ctx)

/** Infer the type of a match expression. */
def inferMatch(match_ : EMatch, ctx: Context): (Type, List[ContextEntry]) =
  // Infer the type and bounds of the scrutinee.
  val (scrutineeType, scrutineeEntries) = infer(match_.scrutinee, ctx)
  val scrutineeCtx = ctx.addEntries(scrutineeEntries)
  // Get the union of the cases.
  val patternsType =
    given Context = scrutineeCtx
    joinMany(match_.cases.map(_.pattern))
  // Constrain the type of the scrutinee to be a subtype of the type of the cases.
  val patternsBounds =
    given Context = scrutineeCtx
    given Mode = Mode.Constrain
    constrainSub(scrutineeType, patternsType)
  val patternsCtx = scrutineeCtx.addEntries(patternsBounds)
  // Infer each match case.
  given Context = patternsCtx
  val (casesType, casesBounds) = match_.cases
    .map(inferMatchCase(_, scrutineeType, patternsCtx))
    .fold1Right((case_, cases) =>
      val (caseType, caseBounds) = case_
      val (casesType, casesBounds) = cases
      val type_ = join(caseType, casesType)
      val bounds = patternsCtx.joinBounds(caseBounds.b, casesBounds.b)
      (type_, bounds)
    )

  (casesType, casesBounds ::: patternsBounds ::: scrutineeEntries)

/** Infer the type of a match case. */
def inferMatchCase(case_ : EMatchCase, scrutineeType: Type, ctx: Context): (Type, List[ContextEntry]) =
  val patternBounds =
    given Context = ctx
    constrainSub(scrutineeType, case_.pattern)
  val caseCtx = ctx.addEntries(patternBounds)
  val (bodyType, bodyBounds) = infer(case_.body, caseCtx)
  (bodyType, bodyBounds ::: patternBounds)
