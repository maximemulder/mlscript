package hkmc2.ctml.core.subtyping

import scala.annotation.tailrec
import scala.collection.mutable

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.structural.sameTypeSyntax
import hkmc2.ctml.core.type_.impls.substitute.substitute
import hkmc2.ctml.types.*

/** Read a producer's explicit witnesses from matching result projections. Every quantified
 *  coordinate needs a defining lower bound and an unambiguous witness. Binders, arrow domains
 *  and Boolean alternatives are opaque. Replay checks every original guard and the result. */
private[subtyping] def subtypeProjectedInstance(scheme: TUniv, target: Type)
    (using ctx: SubContext): Option[SubClauses] =
  val (parameters, guarded) = scheme.getUnivComponents
  val (result, guards) = guarded.getConstrainedComponents
  if guards.isEmpty || result.isInstanceOf[TVar] then return None
  val owners = parameters.toSet
  val defined = guards.iterator.map(g => if g.dir == Direction.Sub then g.right else g.left)
    .collect { case TVar(x) => x }.toSet
  if !owners.subsetOf(defined) || !carrierSyntaxFits(List(guarded, target)) ||
      ctx.trail.instances.contains((scheme, target)) then return None
  val arguments = mutable.Map.empty[TypeVar, Type]

  /** Follow positive projections that occur on both sides of the comparison. Record fields are
   *  matched by label across intersections; several demands for one label are ambiguous. */
  def observe(pattern: Type, wanted: Type): Boolean = (pattern, wanted) match
    case (TVar(x), witness) if owners(x) =>
      sameTypeSyntax(arguments.getOrElseUpdate(x, witness), witness)
    case (TLam(_, r), TLam(_, t)) => observe(r, t)
    case (TTuple(a, b), TTuple(c, d)) => observe(a, c) && observe(b, d)
    case (_: TField | TJointType(JointMode.Inter, _, _), _) =>
      val fields = recordFields(wanted).groupMap(_.name)(_.value)
      recordFields(pattern).forall: field =>
        fields.get(field.name) match
          case Some(witness :: Nil) => observe(field.value, witness)
          case _ => true
    case _ => true

  if !observe(result, target) || arguments.size != parameters.size then return None
  val replay = parameters.foldLeft(Option(scheme: Type)): (instance, x) =>
    instance.flatMap:
      case TUniv(binder, body) =>
        val argument = arguments(x)
        Option.when(carrierSubstitutionFits(List(body), binder, argument)):
          body.substitute(binder, argument)
      case _ => None
  val scoped = ctx.mapTrail(t => t.copy(instances = t.instances + ((scheme, target))))
  replay.flatMap: instance =>
    try Some(subtype(instance, target)(using scoped))
    catch case _: TypeError => None

/** Collect visible fields through intersections without distributing into their payloads. */
private def recordFields(type_ : Type): List[TField] =
  @tailrec
  def collect(todo: List[Type], fields: List[TField]): List[TField] = todo match
    case TJointType(JointMode.Inter, l, r) :: rest => collect(l :: r :: rest, fields)
    case (f: TField) :: rest => collect(rest, f :: fields)
    case _ :: rest => collect(rest, fields)
    case Nil => fields
  collect(List(type_), Nil)
