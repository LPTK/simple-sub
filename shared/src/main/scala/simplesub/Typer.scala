package simplesub

import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap, Set => MutSet}
import scala.collection.immutable.{SortedSet, SortedMap}
import scala.util.chaining._
import scala.annotation.tailrec

final case class TypeError(msg: String) extends Exception(msg)

/** A class encapsulating type inference state.
 *  It uses its own internal representation of types and type variables, using mutable data structures.
 *  Inferred SimpleType values are then turned into CompactType values for simplification (see TypeSimplifier.scala).
 *  In order to turn the resulting CompactType into a simplesub.Type, we use `coalesceCompactType`.
 */
class Typer(protected val dbg: Boolean) extends TyperDebugging {
  
  type Ctx = Map[String, TypeScheme]
  
  val BoolType: Primitive = Primitive("bool")
  val IntType: Primitive = Primitive("int")
  
  val builtins: Ctx = Map(
    "true" -> BoolType,
    "false" -> BoolType,
    "not" -> Function(BoolType, BoolType),
    "succ" -> Function(IntType, IntType),
    "add" -> Function(IntType, Function(IntType, IntType)),
    "if" -> {
      val v = freshVar(1)
      PolymorphicType(0, Function(BoolType, Function(v, Function(v, v))))
    }
  )
  
  /** The main type inference function */
  def inferTypes(pgrm: Pgrm, ctx: Ctx = builtins): List[Either[TypeError, PolymorphicType]] =
    pgrm.defs match {
      case (isrec, nme, rhs) :: defs =>
        val ty_sch = try Right(typeLetRhs(isrec, nme, rhs)(ctx, 0)) catch {
          case err: TypeError => Left(err) }
        ty_sch :: inferTypes(Pgrm(defs), ctx + (nme -> ty_sch.getOrElse(freshVar(0))))
      case Nil => Nil
    }
  
  def inferType(term: Term, ctx: Ctx = builtins, lvl: Int = 0): SimpleType = typeTerm(term)(ctx, lvl)
  
  /** Infer the type of a let binding right-hand side. */
  def typeLetRhs(isrec: Boolean, nme: String, rhs: Term)(implicit ctx: Ctx, lvl: Int): PolymorphicType = {
    val res = if (isrec) {
      val e_ty = freshVar(lvl + 1)
      val ty = typeTerm(rhs)(ctx + (nme -> e_ty), lvl + 1)
      constrain(ty, e_ty)
      e_ty
    } else typeTerm(rhs)(ctx, lvl + 1)
    PolymorphicType(lvl, res)
  }
  
  /** Infer the type of a term. */
  def typeTerm(term: Term)(implicit ctx: Ctx, lvl: Int): SimpleType = {
    lazy val res = freshVar
    term match {
      case Var(name) =>
        ctx.getOrElse(name, err("identifier not found: " + name)).instantiate
      case Lam(name, body) =>
        val param = freshVar
        val body_ty = typeTerm(body)(ctx + (name -> param), lvl)
        Function(param, body_ty)
      case App(f, a) =>
        val f_ty = typeTerm(f)
        val a_ty = typeTerm(a)
        constrain(f_ty, Function(a_ty, res))
        res
      case Lit(n) =>
        IntType
      case Sel(obj, name) =>
        val obj_ty = typeTerm(obj)
        constrain(obj_ty, Record((name, res) :: Nil))
        res
      case Rcd(fs) =>
        Record(fs.map { case (n, t) => (n, typeTerm(t)) })
      case Let(isrec, nme, rhs, bod) =>
        val n_ty = typeLetRhs(isrec, nme, rhs)
        typeTerm(bod)(ctx + (nme -> n_ty), lvl)
    }
  }
  
  /** Constrains the types to enforce a subtyping relationship `lhs` <: `rhs`. */
  def constrain(lhs: SimpleType, rhs: SimpleType)
      // we need a cache to remember the subtyping tests in process; we also make the cache remember
      // past subtyping tests for performance reasons (it reduces the complexity of the algoritghm)
      (implicit cache: MutSet[(SimpleType, SimpleType)] = MutSet.empty)
  : Unit = {
    if (lhs is rhs) return
    val lhs_rhs = lhs -> rhs
    lhs_rhs match {
      // There is no need to remember the subtyping tests performed that did not involve
      // type variables, as type variables will necessary be part of any possible cycles.
      // Since these types form regular trees, there will necessarily be a point where a
      // variable part of a cycle will be matched against the same type periodically.
      case (_: Variable, _) | (_, _: Variable) =>
        if (cache(lhs_rhs)) return
        cache += lhs_rhs
      case _ => ()
    }
    lhs_rhs match {
      case (Function(l0, r0), Function(l1, r1)) =>
        constrain(l1, l0)
        constrain(r0, r1)
      case (Record(fs0), Record(fs1)) =>
        fs1.foreach { case (n1, t1) =>
          fs0.find(_._1 === n1).fold(
            err(s"missing field: $n1 in ${lhs.show}")
          ) { case (n0, t0) => constrain(t0, t1) }
        }
      case (lhs: Variable, rhs) if rhs.level <= lhs.level =>
        lhs.upperBounds ::= rhs
        lhs.lowerBounds.foreach(constrain(_, rhs))
      case (lhs, rhs: Variable) if lhs.level <= rhs.level =>
        rhs.lowerBounds ::= lhs
        rhs.upperBounds.foreach(constrain(lhs, _))
      case (_: Variable, rhs0) =>
        val rhs = extrude(rhs0)(lhs.level, MutMap.empty)
        constrain(lhs, rhs)
      case (lhs0, _: Variable) =>
        val lhs = extrude(lhs0)(rhs.level, MutMap.empty)
        constrain(lhs, rhs)
      case _ => err(s"cannot constrain ${lhs.show} <: ${rhs.show}")
    }
  }
  
  /** Copies a type up to its type variables of wrong level (and their extruded bounds). */
  def extrude(ty: SimpleType)
      (implicit lvl: Int, cache: MutMap[Variable, Variable]): SimpleType =
    if (ty.level <= lvl) ty else ty match {
      case Function(l, r) => Function(extrude(l), extrude(r))
      case Record(fs) => Record(fs.map(nt => nt._1 -> extrude(nt._2)))
      case tv: Variable => cache.getOrElse(tv, {
        val nvs = freshVar
        cache += tv -> nvs
        nvs.lowerBounds = tv.lowerBounds.map(extrude)
        nvs.upperBounds = tv.upperBounds.map(extrude)
        tv.upperBounds ::= nvs
        tv.lowerBounds ::= nvs
        nvs
      })
      case Primitive(_) => ty
    }
  
  def err(msg: String): Nothing = throw TypeError(msg)
  
  private var freshCount = 0
  def freshVar(implicit lvl: Int): Variable = new Variable(lvl, Nil, Nil)
  
  def freshenAbove(lim: Int, ty: SimpleType)(implicit lvl: Int): SimpleType = {
    val freshened = MutMap.empty[Variable, Variable]
    def freshen(ty: SimpleType): SimpleType = if (ty.level <= lim) ty else ty match {
      case tv: Variable =>
        freshened.get(tv) match {
          case Some(tv) => tv
          case None =>
            val v = freshVar
            freshened += tv -> v
            // v.lowerBounds = tv.lowerBounds.mapConserve(freshen)
            // v.upperBounds = tv.upperBounds.mapConserve(freshen)
            //  ^ the above are more efficient, but they lead to a different order
            //    of fresh variable creations, which leads to some types not being
            //    simplified the same when put into the RHS of a let binding...
            v.lowerBounds = tv.lowerBounds.reverse.map(freshen).reverse
            v.upperBounds = tv.upperBounds.reverse.map(freshen).reverse
            v
        }
      case Function(l, r) => Function(freshen(l), freshen(r))
      case Record(fs) => Record(fs.map(ft => ft._1 -> freshen(ft._2)))
      case Primitive(_) => ty
    }
    freshen(ty)
  }
  
  
  // The data types used for type inference:
  
  /** A type that potentially contains universally quantified type variables,
   *  and which can be isntantiated to a given level. */
  sealed abstract class TypeScheme {
    def instantiate(implicit lvl: Int): SimpleType
  }
  /** A type with universally quantified type variables
   *  (by convention, those variables of level greater than `level` are considered quantified). */
  case class PolymorphicType(level: Int, body: SimpleType) extends TypeScheme {
    def instantiate(implicit lvl: Int) = freshenAbove(level, body)
  }
  /** A type without universally quantified type variables. */
  sealed abstract class SimpleType extends TypeScheme with SimpleTypeImpl {
    def level: Int
    def instantiate(implicit lvl: Int) = this
  }
  case class Function(lhs: SimpleType, rhs: SimpleType) extends SimpleType {
    lazy val level: Int = lhs.level max rhs.level
    override def toString = s"($lhs -> $rhs)"
  }
  case class Record(fields: List[(String, SimpleType)]) extends SimpleType {
    lazy val level: Int = fields.iterator.map(_._2.level).maxOption.getOrElse(0)
    override def toString = s"{${fields.map(f => s"${f._1}: ${f._2}").mkString(", ")}}"
  }
  case class Primitive(name: String) extends SimpleType {
    def level: Int = 0
    override def toString = name
  }
  /** A type variable living at a certain polymorphism level `level`, with mutable bounds.
   *  Invariant: Types appearing in the bounds never have a level higher than this variable's `level`. */
  final class Variable(
      val level: Int,
      var lowerBounds: List[SimpleType],
      var upperBounds: List[SimpleType],
  ) extends SimpleType with CompactTypeOrVariable {
    private[simplesub] val uid: Int = { freshCount += 1; freshCount - 1 }
    private[simplesub] var recursiveFlag = false // used temporarily by `compactType`
    lazy val asTypeVar = new TypeVariable("α", uid)
    override def toString: String = "α" + uid + "'" * level
    override def hashCode: Int = uid
  }
  
  trait CompactTypeOrVariable
  
  
  type PolarVariable = (Variable, Boolean)
  
  /** Convert an inferred SimpleType into the immutable Type representation. */
  def coalesceType(st: SimpleType): Type = {
    val recursive = mutable.Map.empty[PolarVariable, TypeVariable]
    def go(st: SimpleType, polarity: Boolean)(inProcess: Set[PolarVariable]): Type = st match {
      case tv: Variable =>
        val tv_pol = tv -> polarity
        if (inProcess.contains(tv_pol))
          recursive.getOrElseUpdate(tv_pol, freshVar(0).asTypeVar)
        else {
          val bounds = if (polarity) tv.lowerBounds else tv.upperBounds
          val boundTypes = bounds.map(go(_, polarity)(inProcess + tv_pol))
          val mrg = if (polarity) Union else Inter
          val res = boundTypes.foldLeft[Type](tv.asTypeVar)(mrg)
          recursive.get(tv_pol).fold(res)(RecursiveType(_, res))
        }
      case Function(l, r) => FunctionType(go(l, !polarity)(inProcess), go(r, polarity)(inProcess))
      case Record(fs) => RecordType(fs.map(nt => nt._1 -> go(nt._2, polarity)(inProcess)))
      case Primitive(n) => PrimitiveType(n)
    }
    go(st, true)(Set.empty)
  }
  
  
}
