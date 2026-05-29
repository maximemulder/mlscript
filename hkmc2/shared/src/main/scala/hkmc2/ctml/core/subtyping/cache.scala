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

    // val result = this.pairs.contains((sub, sup))
    //val result = (sub, sup) match
    //  case (TVar(_), TVar(_)) =>
    //      this.pairs.contains((sub, sup))
    //  case (TVar(_), sup) =>
    //      this.pairs.contains((sub, sup.shadow))
    //  case (sub, TVar(_)) =>
    //      this.pairs.contains((sub.shadow, sup))
    //  case _ =>
    //      this.pairs.contains((sub, sup))

    val result = this.pairs.contains(sub.shadow, sup.shadow)

    if config.debug.cacheHit && result then
      output(s"CACHE HIT ${sub} ${sup}")

    if config.debug.cacheMiss && !result then
      output(s"CACHE MISS ${sub} ${sup}")

//    if result then
//      return true
//
//    sub match
//      case TVar(var_) =>
//        ctx.clauses.typeVarDecls.find(_.var_ == var_).flatMap(_.origin) match
//          case Some(parent) if this.check(TVar(parent), sup) =>
//            return true
//          case _ =>
//            ()
//      case _ =>
//        ()
//
//    sup match
//      case TVar(var_) =>
//        ctx.clauses.typeVarDecls.find(_.var_ == var_).flatMap(_.origin) match
//          case Some(parent) if this.check(sub, TVar(parent)) =>
//            return true
//          case _ =>
//            ()
//      case _ =>
//        ()

    result

  /** Add two types to the subtyping cache according to the type checker configuration. */
  def add(sub: Type, sup: Type)(using ctx: Context): SubtypingCache =
    config.cacheMode match
      case CacheMode.Var =>
        (sub, sup) match
          case (TVar(_), TVar(_)) =>
            this.addImpl(sub, sup)
          case (TVar(_), sup) =>
            this.addImpl(sub, sup.shadow)
          case (sub, TVar(_)) =>
            this.addImpl(sub.shadow, sup)
          // Cache only when one of the types is a type variable.
          // case (TVar(var_), _) =>
          //   val a = this.addImpl(TVar(var_), sup)
          //   ctx.getTypeVarOriginal(var_) match
          //     case Some(original) =>
          //       a.addImpl(TVar(original), sup)
          //     case None =>
          //       a
          // case (_, TVar(var_)) =>
          //   val a = this.addImpl(sub, TVar(var_))
          //   ctx.getTypeVarOriginal(var_) match
          //     case Some(original) =>
          //       a.addImpl(sub, TVar(original))
          //     case None =>
          //       a
          // Do not cache otherwhile.
          case _ =>
            this
      // Cache in all cases.
      case CacheMode.All =>
        // this.addImpl(sub, sup)
        this.addImpl(sub.shadow, sup.shadow)
        //(sub, sup) match
        //  case (TVar(_), TVar(_)) =>
        //    this.addImpl(sub, sup)
        //  case (TVar(_), sup) =>
        //    this.addImpl(sub, sup.shadow)
        //  case (sub, TVar(_)) =>
        //    this.addImpl(sub.shadow, sup)
        //  case _ =>
        //    this

  /** Add two types to the subtyping cache unconditionally. */
  def addImpl(sub: Type, sup: Type): SubtypingCache =
    if config.debug.cacheAdd then
      output(s"CACHE ADD ${sub} ${sup}")

    SubtypingCache(this.pairs + ((sub, sup)))
