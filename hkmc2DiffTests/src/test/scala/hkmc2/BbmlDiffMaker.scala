package hkmc2

import mlscript.utils.*, shorthands.*

import hkmc2.semantics.*
import hkmc2.bbml.*
import utils.Scope


abstract class BbmlDiffMaker extends JSBackendDiffMaker:
  val bbPreludeFile = file / os.up / os.RelPath("bbPrelude.mls")

  /** The CTML prelude file path. */
  val ctmlPreludeFilePath = file / os.up / os.RelPath("ctmlPrelude.mls")

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
      hkmc2.ctml.core.debug.DebugInfo.reset()
      if file =/= ctmlPreludeFilePath then
        curCtx = Elaborator.State.init
        given Config = mkConfig
        importFile(ctmlPreludeFilePath, verbose = false)

  /** The CTML debug command. */
  val ctmlDbgOpt = new Command("ctml-dbg")(line =>
    val flags = line.split(" ")
    for flag <- flags do
      if !hkmc2.ctml.core.debug.DebugFlags.applyFlag(flag) then
        output(s"Unknown CTML debug term '${flag}'.")
  )

  /** The CTML typing context. */
  var ctmlCtx = hkmc2.ctml.types.Context.none

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
