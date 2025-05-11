package hkmc2

import mlscript.utils.*, shorthands.*

import hkmc2.semantics.*
import hkmc2.bbml.*
import hkmc2.ctml.core.show
import hkmc2.ctml.types.Context
import hkmc2.ctml.types.Ok
import hkmc2.ctml.types.Type
import utils.Scope


abstract class BbmlDiffMaker extends JSBackendDiffMaker:

  val bbPreludeFile = file / os.up / os.RelPath("bbPrelude.mls")

  val bbmlOpt = new NullaryCommand("bbml"):
    override def onSet(): Unit =
      super.onSet()
      noSanityCheck.isGlobal = true
      noSanityCheck.set
      if file =/= bbPreludeFile then
        curCtx = Elaborator.State.init
        given Config = mkConfig
        importFile(bbPreludeFile, verbose = false)

  /** Constraint types command. */
  val ctmlOpt = new NullaryCommand("ctml")

  override def init(): Unit =
    super.init()

  lazy val bbCtx =
    given Elaborator.Ctx = curCtx
    bbml.BbCtx.init(_ => die)

  var bbmlTyper: Opt[BBTyper] = None


  override def processTerm(term: semantics.Term.Blk, inImport: Bool)(using Config, Raise): Unit =
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
      ctml.core.freshVarCounter = 0
      var res = ctml.core.infer(Context.empty, term)
      res match
        case ok: Ok[Type] =>
          output(ok.value.show())
        case _ =>
          output("Type checking error.")
