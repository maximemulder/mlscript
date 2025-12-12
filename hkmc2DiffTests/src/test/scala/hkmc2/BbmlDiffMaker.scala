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
  val ctmlCommand = new NullaryCommand("ctml"):
    override def onSet(): Unit =
      super.onSet()
      // Assign the global CTML fresh variable counter.
      hkmc2.ctml.core.var_.freshVarCounter = 0
      hkmc2.ctml.core.config.config = hkmc2.ctml.core.config.Config()
      if file =/= ctmlPreludeFilePath then
        curCtx = Elaborator.State.init
        given Config = mkConfig
        importFile(ctmlPreludeFilePath, verbose = false)

  /** The CTML configuration command. */
  val ctmlMergeModeCommand = new Command("ctml-cfg")(line =>
    hkmc2.ctml.core.config.applyConfigArguments(line.split(" ").toList)
  )

  /** The CTML debug command. */
  val ctmlDebugCommand = new Command("ctml-dbg")(line =>
    hkmc2.ctml.core.config.applyDebugArguments(line.split(" ").toList)
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

    if ctmlCommand.isSet then
      this.ctmlCtx = hkmc2.ctml.test.test(term, this.ctmlCtx, inImport, output.apply, raise)
