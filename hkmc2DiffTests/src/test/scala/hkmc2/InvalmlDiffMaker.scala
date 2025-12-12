package hkmc2

import hkmc2.utils.*, shorthands.*

import hkmc2.semantics.*
import hkmc2.invalml.*
import utils.Scope


abstract class InvalMLDiffMaker extends JSBackendDiffMaker:

  val invalPreludeFile = io.Path(rootPath) / "hkmc2" / "shared" / "src" / "test" / "mlscript" / "invalml" / "InvalMLPrelude.mls"

  val invalmlOpt = new NullaryCommand("invalml"):
    override def onSet(): Unit =
      super.onSet()
      noSanityCheck.isGlobal = true
      noSanityCheck.set
      if file =/= invalPreludeFile then
        curCtx = Elaborator.State.init
        given Config = mkConfig
        importFile(invalPreludeFile, verbose = false)


  /** The CTML prelude file path. */
  val ctmlPreludeFilePath = io.Path(rootPath) / "hkmc2" / "shared" / "src" / "test" / "mlscript" / "ctml" / "ctmlPrelude.mls"

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

  lazy val invalCtx =
    given Elaborator.Ctx = curCtx
    invalml.InvalCtx.init(_ => die)

  var invalmlTyper: Opt[InvalTyper] = None

  override def processTerm(term: semantics.Term.Blk, inImport: Bool)(using ctx: Config, raise: Raise): Unit =
    super.processTerm(term, inImport)
    if invalmlOpt.isSet then
      given Scope = Scope.empty(Scope.Cfg.default)
      if invalmlTyper.isEmpty then
        given Elaborator.Ctx = curCtx
        invalmlTyper = S(InvalTyper())
      given hkmc2.invalml.InvalCtx = invalCtx.copy(raise = summon)
      val typer = invalmlTyper.get
      val ty = typer.typePurely(term)
      val printer = PrettyPrinter((msg: String) => output(msg))
      if debug.isSet then printer.print(ty)
      val simplif = TypeSimplifier(tl)
      val sty = simplif(true, 0)(ty)
      printer.print(sty)

    if ctmlCommand.isSet then
      this.ctmlCtx = hkmc2.ctml.test.test(term, this.ctmlCtx, inImport, output.apply, raise)
