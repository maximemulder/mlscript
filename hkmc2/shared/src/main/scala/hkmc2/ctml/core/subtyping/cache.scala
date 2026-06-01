package hkmc2.ctml.core.subtyping

import hkmc2.ctml.config.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.clauses.typeVarDecls

/** Cache used to store, detect, and solve recursive subtyping queries. */
case class SubtypingCache(pairs: Set[(Type, Type)] = Set()):
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

  private def checkInner(sub: Type, sup: Type)(using ctx: Context): Boolean =
    config.cacheMode match
      case CacheMode.Var =>
        sub match
          case TVar(var_) if this.pairs.contains(this.shadow(var_), sup) =>
            return true
          case _ =>
            ()

        sup match
          case TVar(var_) if this.pairs.contains(sub, this.shadow(var_)) =>
            return true
          case _ =>
            ()

        false
      case CacheMode.All =>
        this.pairs.contains(sub, sup)

  private def addInner(sub: Type, sup: Type)(using ctx: Context): SubtypingCache =
    config.cacheMode match
      case CacheMode.Var =>
        var cache = this

        sub match
          case TVar(var_) =>
            cache = SubtypingCache(this.pairs + ((this.shadow(var_), sup)))
          case _ =>
            ()

        sup match
          case TVar(var_) =>
            cache = SubtypingCache(this.pairs + ((sub, this.shadow(var_))))
          case _ =>
            ()

        cache
      // Cache in all cases.
      case CacheMode.All =>
        SubtypingCache(this.pairs + ((sub, sup)))

  private def shadow(var_ : TypeVar)(using ctx: Context): Type =
    if config.cacheShadow then
      TVar(var_.shadow)
    else
      TVar(var_)
