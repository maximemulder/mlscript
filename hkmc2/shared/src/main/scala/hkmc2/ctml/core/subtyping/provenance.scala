package hkmc2.ctml.core.subtyping

import scala.collection.mutable

import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*

/** A bounded description of a judgment. Equal keys are search collisions, not equal judgments.
 *  Free input variables and rigid variables retain their concrete identities. Other instances
 *  retain their origin, creation role, abstract flow context, and within-key aliasing pattern.
 *  Side bounds distinguish some changes in assumptions without following their transitive graph.
 *  Neither these selected bounds nor the truncated syntax describe the entire proof context. */
case class ProvenanceKey(parts: Vector[Vector[ProvenanceToken]])

/** Tokens of the search abstraction, separate from the type language and its equality. */
enum ProvenanceToken:
  /** A type constructor or part delimiter. */
  case Shape(tag: String)
  /** A record field or class name. */
  case Label(name: String)
  /** An anchored free variable with its current kind. */
  case Fixed(name: String, kind: TypeVarKind)
  /** A locally numbered instance; numbering preserves distinct variables with equal origins. */
  case Instance(index: Int, provenance: VarProvenance)
  /** A locally bound occurrence, with a de Bruijn index. */
  case Bound(index: Int)
  /** A binder's stable origin. */
  case Binder(origin: VarOrigin)
  /** The remainder of a type beyond the key's syntax budget. */
  case Truncated

/** Construction of bounded, provenance-sensitive search keys. */
object ProvenanceKey:
  /** Maximum structural depth, independent of the number of visited nodes. A node limit alone
   *  follows one branch of a repeatedly doubled arrow until ordinary type hashing is enormous. */
  private val maxDepth = 8

  /** Serialize both sides and their direct variable bounds with independent syntax budgets.
   *  A common numbering table preserves sharing across all parts. Scopes are lexical within
   *  types; rigid variables are never abstractly identified across quantifier introductions. */
  def apply(sub: Type, sup: Type, roots: Set[TypeVar])(using ctx: SubContext): ProvenanceKey =
    val instances = mutable.Map.empty[TypeVar, Int]
    val declarations = ctx.typeVarDecls.toList.reverseIterator.map(decl => decl.var_ -> decl).toMap

    /** Encode one bounded syntax tree without recursively expanding variable bounds. */
    def part(type_ : Type, limit: Int): Vector[ProvenanceToken] =
      val tokens = Vector.newBuilder[ProvenanceToken]
      var remaining = limit

      /** Visit at most `limit` nodes, retaining a distinct truncation marker. */
      def visit(type_ : Type, bound: List[TypeVar], depth: Int): Unit =
        import ProvenanceToken.*
        if remaining <= 0 || depth >= maxDepth then tokens += Truncated
        else
          remaining -= 1
          type_ match
            case TVar(variable) =>
              val index = bound.indexOf(variable)
              if index >= 0 then tokens += ProvenanceToken.Bound(index)
              else
                val decl = declarations.getOrElse(variable,
                  throw IllegalStateException(s"Undeclared variable in provenance key: $variable"))
                if roots(variable) || decl.kind == TypeVarKind.Rigid ||
                    decl.var_.provenance.role == VarRole.Root then
                  tokens += Fixed(variable.name, decl.kind)
                else
                  val number = instances.getOrElseUpdate(variable, instances.size)
                  tokens += Instance(number, decl.var_.provenance)
            case TUniv(variable, body) =>
              tokens += Binder(variable.provenance.root)
              visit(body, variable :: bound, depth + 1)
            case TBot => tokens += Shape("bottom")
            case TTop => tokens += Shape("top")
            case TClass(variable) =>
              tokens += Shape("class")
              tokens += Label(variable.name)
            case TField(name, value) =>
              tokens += Shape("field")
              tokens += Label(name)
              visit(value, bound, depth + 1)
            case TConstrained(body, constraint) =>
              tokens += Shape(s"guard:${constraint.dir}")
              visit(constraint.left, bound, depth + 1)
              visit(constraint.right, bound, depth + 1)
              visit(body, bound, depth + 1)
            case _ =>
              val tag = type_ match
                case TNeg(_) => "negation"
                case TLam(_, _) => "arrow"
                case TTuple(_, _) => "tuple"
                case TApp(_, _) => "application"
                case TJointType(mode, _, _) => s"joint:$mode"
                case _ => throw IllegalStateException("Missing provenance constructor")
              tokens += Shape(tag)
              type_.components.foreach(visit(_, bound, depth + 1))
      visit(type_, Nil, 0)
      tokens.result()

    val sides = List(sub.lookupVar, sup.lookupVar).flatten.distinct
    val bounds = sides.flatMap(variable => List(variable.lowerBound, variable.upperBound))
    ProvenanceKey(Vector(part(sub, 128), part(sup, 128)) ++ bounds.map(part(_, 64)))
