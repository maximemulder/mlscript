package hkmc2.ctml.core.subtyping

import hkmc2.ctml.config.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.clauses.typeVarDecls

/** Cache used to store, detect, and solve recursive subtyping queries. */
case class SubtypingCache(
  /** The type variable cache. */
  val vars: Set[(TypeVar, Direction, Type)] = Set(),
  /** The full type cache. */
  val types: Set[(Type, Type)] = Set(),
  /** The universal type cache. */
  val univs: Map[(TypeVar, Type), TypeVar] = Map(),
):
  /** Check whether two types are in the subtyping cache. */
  def check(sub: Type, sup: Type)(using ctx: Context): Boolean =
    if config.debug.cacheCheck then
      output(s"CACHE CHECK ${sub} ${sup}")

    val result = checkInner(sub, sup)

    if config.debug.cacheHit && result then
      output(s"CACHE HIT ${sub} ${sup}")

    if config.debug.cacheMiss && !result then
      output(s"CACHE MISS ${sub} ${sup}")

    result

  /** Add two types to the subtyping cache according to the type checker configuration. */
  def add(sub: Type, sup: Type)(using ctx: Context): SubtypingCache =
    if config.debug.cacheAdd then
      output(s"CACHE ADD ${sub} ${sup}")

    this.addInner(sub, sup)

  def checkUniv(var_ : TypeVar, type_ : Type): Option[TypeVar] =
    this.univs.get((var_, type_))

  def addUniv(var_ : TypeVar, type_ : Type, fresh: TypeVar): SubtypingCache =
    SubtypingCache(this.vars, this.types, this.univs + ((var_, type_) -> fresh))

  private def checkInner(sub: Type, sup: Type)(using ctx: Context): Boolean =
    if config.cacheVar then
      sub match
        case TVar(var_) if this.vars.contains((this.shadow(var_), Direction.Sub, sup)) =>
          return true
        case _ =>
          ()

      sup match
        case TVar(var_) if this.vars.contains((this.shadow(var_), Direction.Super, sub)) =>
          return true
        case _ =>
          ()

    if config.cacheType then
      if this.types.contains(sub, sup) then
        return true

    false

  private def addInner(sub: Type, sup: Type)(using ctx: Context): SubtypingCache =
    var cache = this

    if config.cacheVar then

        sub match
          case TVar(var_) =>
            cache = SubtypingCache(this.vars + ((this.shadow(var_), Direction.Sub, sup)), this.types, this.univs)
          case _ =>
            ()

        sup match
          case TVar(var_) =>
            cache = SubtypingCache(this.vars + ((this.shadow(var_), Direction.Super, sub)), this.types, this.univs)
          case _ =>
            ()

        cache
      // Cache in all cases.
    if config.cacheType then
      cache = SubtypingCache(this.vars, this.types + ((sub, sup)), this.univs)

    cache

  private def shadow(var_ : TypeVar)(using ctx: Context): TypeVar =
    if config.cacheShadow then
      var_.shadow
    else
      var_
