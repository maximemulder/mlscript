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
    val parts = line.split(" ")
    for part <- parts do
      part match
        case "" =>
          hkmc2.ctml.core.debug.DebugInfo.reset()
        case "infer" =>
          hkmc2.ctml.core.debug.DebugInfo.infer     = true
        case "constrain" =>
          hkmc2.ctml.core.debug.DebugInfo.constrain = true
        case "check" =>
          hkmc2.ctml.core.debug.DebugInfo.check     = true
        case "join" =>
          hkmc2.ctml.core.debug.DebugInfo.join      = true
        case "meet" =>
          hkmc2.ctml.core.debug.DebugInfo.meet      = true
        case "var" =>
          hkmc2.ctml.core.debug.DebugInfo.var_      = true
        case _ =>
          output(s"Unknown CTML debug term '${part}'.")
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

    if ctmlOpt.isSet then
      this.ctmlCtx = hkmc2.ctml.test.test(term, this.ctmlCtx, inImport, output.apply, raise)
