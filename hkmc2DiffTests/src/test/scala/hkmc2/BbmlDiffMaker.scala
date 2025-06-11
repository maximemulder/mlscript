package hkmc2

import mlscript.utils.*, shorthands.*

import hkmc2.semantics.*
import hkmc2.bbml.*
import utils.Scope


abstract class BbmlDiffMaker extends JSBackendDiffMaker:
  val bbPreludeFile = file.up / "bbPrelude.mls"

  /** The CTML prelude file path. */
  val ctmlPreludeFilePath = file.up / "ctmlPrelude.mls"

  val bbmlOpt = new NullaryCommand("bbml"):
    override def onSet(): Unit =
      super.onSet()
      noSanityCheck.isGlobal = true
      noSanityCheck.set
      if file =/= bbPreludeFile then
        curCtx = Elaborator.State.init
        given Config = mkConfig
        importFile(bbPreludeFile, verbose = false)

  /** The CTML command. */
  val ctmlOpt = new NullaryCommand("ctml"):
    override def onSet(): Unit =
      super.onSet()
      // Assign the global CTML fresh variable counter.
      hkmc2.ctml.core.freshVarCounter = 0
      if file =/= ctmlPreludeFilePath then
        curCtx = Elaborator.State.init
        given Config = mkConfig
        importFile(ctmlPreludeFilePath, verbose = false)

  /** The CTML debug command. */
  val ctmlDbgOpt = new Command("ctml-dbg")(line =>
    val parts = line.split(" ")
    for part <- parts do
      part match
        case "" =>
          hkmc2.ctml.core.inferDebugFlag     = false
          hkmc2.ctml.core.constrainDebugFlag = false
          hkmc2.ctml.core.checkDebugFlag     = false
          hkmc2.ctml.core.joinDebugFlag      = false
          hkmc2.ctml.core.meetDebugFlag      = false
        case "infer" =>
          hkmc2.ctml.core.inferDebugFlag     = true
        case "constrain" =>
          hkmc2.ctml.core.constrainDebugFlag = true
        case "check" =>
          hkmc2.ctml.core.checkDebugFlag     = true
        case "join" =>
          hkmc2.ctml.core.joinDebugFlag      = true
        case "meet" =>
          hkmc2.ctml.core.meetDebugFlag      = true
        case _ =>
          output(s"Unknown CTML debug term '${part}'.")
  )

  /** The CTML typing context. */
  var ctmlCtx = hkmc2.ctml.types.Clauses()

  override def init(): Unit =
    super.init()

  lazy val bbCtx =
    given Elaborator.Ctx = curCtx
    bbml.BbCtx.init(_ => die)

  var bbmlTyper: Opt[BBTyper] = None

  override def processTerm(term: semantics.Term.Blk, inImport: Bool)(using ctx: Config, raise: Raise): Unit =
    super.processTerm(term, inImport)
    if bbmlOpt.isSet then
      given Scope = Scope.empty
      if bbmlTyper.isEmpty then
        given Elaborator.Ctx = curCtx
        bbmlTyper = S(BBTyper())
      given hkmc2.bbml.BbCtx = bbCtx.copy(raise = summon)
      val typer = bbmlTyper.get
      val ty = typer.typePurely(term)
      val printer = PrettyPrinter((msg: String) => output(msg))
      if debug.isSet then printer.print(ty)
      val simplif = TypeSimplifier(tl)
      val sty = simplif(true, 0)(ty)
      printer.print(sty)

    if ctmlOpt.isSet then
      this.ctmlCtx = hkmc2.ctml.test.test(term, this.ctmlCtx, inImport, output.apply, raise)
