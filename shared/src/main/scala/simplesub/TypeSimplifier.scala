package simplesub

import scala.collection.mutable.{Map => MutMap, Set => MutSet, LinkedHashMap, LinkedHashSet}
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.util.chaining._

trait TypeSimplifier { self: Typer =>
  
  
  // Compact types representation, useful for simplification:
  
  /** Depending on whether it occurs in positive or negative position,
   * this represents either a union or an intersection, respectively,
   * of different type components.  */
  case class CompactType(
    vars: Set[Variable],
    prims: Set[Primitive],
    rec: Option[SortedMap[String, CompactType]],
    fun: Option[(CompactType, CompactType)])
  extends CompactTypeOrVariable {
    def isEmpty: Boolean = vars.isEmpty && prims.isEmpty && rec.isEmpty && fun.isEmpty
    override def toString = List(vars, prims,
      rec.map(_.map(fs => fs._1 + ": " + fs._2).mkString("{",", ","}")),
      fun.map(lr => lr._1.toString + " -> " + lr._2),
    ).flatten.mkString("‹", ", ", "›")
  }
  object CompactType {
    val empty: CompactType = CompactType(Set.empty, Set.empty, None, None)
    def merge(pol: Boolean)(lhs: CompactType, rhs: CompactType): CompactType = {
      val rec = mergeOptions(lhs.rec, rhs.rec) { (lhs, rhs) =>
        if (pol) lhs.flatMap { case (k, v) => rhs.get(k).map(k -> merge(pol)(v, _)) }
        else mergeSortedMap(lhs, rhs)(merge(pol)) }
      val fun = mergeOptions(lhs.fun, rhs.fun){
        case ((l0,r0), (l1,r1)) => (merge(!pol)(l0, l1), merge(pol)(r0, r1)) }
      CompactType(lhs.vars ++ rhs.vars, lhs.prims ++ rhs.prims, rec, fun)
    }
  }
  
  case class CompactTypeScheme(term: CompactType, recVars: Map[Variable, CompactType])
  
  
  // Simplification:
  
  // It is best to convert inferred types into compact types so we merge the bounds of inferred variables
  //   e.g., transform the bounds {x: A}, {x: B; y: C} into {x: A ∧ B; y: C}
  // This will allow for more precise co-occurrence analysis and hash-consing down the line.
  
  /** Convert an inferred SimpleType into the CompactType representation for simplification. */
  def compactType(ty: SimpleType): CompactTypeScheme = {
    import CompactType.{empty, merge}, empty.{copy => ct}
    
    val recursive = MutMap.empty[PolarVariable, Variable]
    var recVars = SortedMap.empty[Variable, CompactType](Ordering by (_.uid))
    
    // `parents` remembers the variables whose bounds are being compacted,
    //    so as to remove spurious cycles such as ?a <: ?b and ?b <: ?a,
    //    which do not correspond to actual recursive types.
    def go(ty: SimpleType, pol: Boolean, parents: Set[Variable])
          (implicit inProcess: Set[PolarVariable]): CompactType = ty match {
      case p: Primitive => ct(prims = Set(p))
      case Function(l, r) =>
        ct(fun = Some(go(l, !pol, Set.empty) -> go(r, pol, Set.empty)))
      case Record(fs) =>
        ct(rec = Some(SortedMap from fs.iterator.map(f => f._1 -> go(f._2, pol, Set.empty))))
      case tv: Variable =>
        val bounds = if (pol) tv.lowerBounds else tv.upperBounds
        val tv_pol = tv -> pol
        if (inProcess.contains(tv_pol))
          if (parents(tv)) ct() // we have a spurious cycle: ignore the bound
          else ct(vars = Set(recursive.getOrElseUpdate(tv_pol, freshVar(0))))
        else (inProcess + tv_pol) pipe { implicit inProcess =>
          val bound = bounds.map(b => go(b, pol, parents + tv))
            .foldLeft[CompactType](ct(vars = Set(tv)))(merge(pol))
          recursive.get(tv_pol) match {
            case Some(v) =>
              recVars += v -> bound
              ct(vars = Set(v))
            case None => bound
          }
        }
    }
    
    CompactTypeScheme(go(ty, true, Set.empty)(Set.empty), recVars)
  }
  
  /** Like `compactType` (convert SimpleType to CompactType), but also make sure to produce a 'canonicalized' compact type,
   * that is: a type where all co-occurring recursive types are merged — and if they have different cycle lengths,
   * we create a new recursive type whose cycle length is the LCD of the respective original cycle lengths.
   * To do this, while detecting recursive types, we keep track of _sets_ of traversed variables (instead of single
   * variables, as in `compactType`).
   * This requires an interleaved two-phase process where we first transitively merge all bounds of all co-occurring
   * variables in the outer layer of the source type, and then traverse the resulting sets of variables further.
   * This "unrolling of recursive types until they align" process is akin to the powerset construction for
   * turning NFAs into DFAs, and can in principle result in an exponentially larger type, though in practice the
   * algorithm does often result in useful simplification down the line (for instance, see [test:T2]). */
  def canonicalizeType(ty: SimpleType): CompactTypeScheme = {
    import CompactType.{empty, merge}, empty.{copy => ct}
    
    type PolarVariables = (Set[Variable], Boolean)
    val recursive = MutMap.empty[PolarVariables, Variable]
    var recVars = SortedMap.empty[Variable, CompactType](Ordering by (_.uid))
    
    // Turn the outermost layer of a SimpleType into a CompactType, leaving type variables untransformed
    def go0(ty: SimpleType, pol: Boolean): CompactType = ty match {
      case p: Primitive => ct(prims = Set(p))
      case Function(l, r) =>
        ct(fun = Some(go0(l, !pol) -> go0(r, pol)))
      case Record(fs) =>
        ct(rec = Some(SortedMap from fs.iterator.map(f => f._1 -> go0(f._2, pol))))
      case tv: Variable =>
        val tvs = closeOver(Set(tv)) {
          case tv1: Variable =>
            val bounds = if (pol) tv1.lowerBounds else tv1.upperBounds
            bounds.iterator.collect{ case v: Variable => v }.toSet
          case _ => Set.empty
        }
        ct(vars = tvs)
    }
    
    // Merge the bounds of all type variables of the given CompactType, and traverse the result
    def go1(ty: CompactType, pol: Boolean)
           (implicit inProcess: Set[PolarVariables]): CompactType = if (ty.isEmpty) ty else {
      val tvs = ty.vars -> pol
      if (inProcess.contains(tvs))
        ct(vars = Set(recursive.getOrElseUpdate(tvs, freshVar(0))))
      else {
        val bound = ty.vars.iterator.flatMap { tv =>
          val bounds = if (pol) tv.lowerBounds else tv.upperBounds
          bounds.iterator.map {
            case _: Variable => empty
            case b => go0(b, pol)
          }
        }.reduceOption(merge(pol)).getOrElse(empty)
        val res = merge(pol)(ty, bound)
        (if (ty.vars.isEmpty) inProcess else inProcess + tvs) pipe { implicit inProcess =>
          val adapted = CompactType(
            res.vars, res.prims,
            res.rec.map(fs => fs.map(kv => kv._1 -> go1(kv._2, pol))),
            res.fun.map(lr => go1(lr._1, !pol) -> go1(lr._2, pol)),
          )
          recursive.get(tvs) match {
            case Some(v) =>
              recVars += v -> adapted
              ct(vars = Set(v))
            case None => adapted
          }
        }
      }
    }
    
    CompactTypeScheme(go1(go0(ty, true), true)(Set.empty), recVars)
  }
  
  
  // Idea: if a type var 'a always occurs positively (resp. neg) along with some 'b AND vice versa,
  //      this means that the two are undistinguishable, and they can therefore be unified.
  //    Ex: ('a & 'b) -> ('a, 'b) is the same as 'a -> ('a, 'a)
  //    Ex: ('a & 'b) -> 'b -> ('a, 'b) is NOT the same as 'a -> 'a -> ('a, 'a)
  //      there is no value of 'a that can make 'a -> 'a -> ('a, 'a) <: (a & b) -> b -> (a, b) work
  //      we'd require 'a :> b | a & b <: a & b, which are NOT valid bounds!
  //    Ex: 'a -> 'b -> 'a | 'b is the same as 'a -> 'a -> 'a
  //    Justification: the other var 'b can always be taken to be 'a & 'b (resp. a | b)
  //      without loss of gen. Indeed, on the pos side we'll have 'a <: 'a & 'b and 'b <: 'a & 'b
  //      and on the neg side, we'll always have 'a and 'b together, i.e., 'a & 'b
  
  // Additional idea: remove variables which always occur both positively AND negatively together
  //      with some other type.
  //    This would arise from constraints such as: 'a :> Int <: 'b and 'b <: Int
  //      (contraints which basically say 'a =:= 'b =:= Int)
  //    Ex: 'a ∧ Int -> 'a ∨ Int is the same as Int -> Int
  //    Currently, we only do this for primitive types PrimType.
  //    In principle it could be done for functions and records, too.
  //    Note: conceptually, this idea subsumes the simplification that removes variables occurring
  //        exclusively in positive or negative positions.
  //      Indeed, if 'a never occurs positively, it's like it always occurs both positively AND
  //      negatively along with the type Bot, so we can replace it with Bot.
  
  /** Simplifies a CompactTypeScheme by performing a co-occurrence analysis on the type variables. */
  def simplifyType(cty: CompactTypeScheme): CompactTypeScheme = {
    
    // State accumulated during the analysis phase:
    var allVars = SortedSet.from(cty.recVars.keysIterator)(Ordering by (-_.uid))
    var recVars = SortedMap.empty[Variable, () => CompactType](Ordering by (_.uid))
    val coOccurrences: MutMap[(Boolean, Variable), MutSet[SimpleType]] = LinkedHashMap.empty
    
    // This will be filled up after the analysis phase, to influence the reconstruction phase:
    val varSubst = MutMap.empty[Variable, Option[Variable]]
    
    // Traverses the type, performing the analysis, and returns a thunk to reconstruct it later:
    def go(ty: CompactType, pol: Boolean): () => CompactType = {
      ty.vars.foreach { tv =>
        allVars += tv
        val newOccs = LinkedHashSet.from(ty.vars.iterator ++ ty.prims)
        coOccurrences.get(pol -> tv) match {
          case Some(os) => os.filterInPlace(newOccs) // computes the intersection
          case None => coOccurrences(pol -> tv) = newOccs
        }
        cty.recVars.get(tv).foreach { b => // if `tv` is recursive, process its bound `b` too
          if (!recVars.contains(tv)) {
            lazy val goLater: () => CompactType = {
              // Make sure to register `tv` before recursing, to avoid an infinite recursion:
              recVars += tv -> (() => goLater())
              go(b, pol)
            }; goLater
          }; ()
        }
      }
      val rec_ = ty.rec.map(_.map(f => f._1 -> go(f._2, pol)))
      val fun_ = ty.fun.map(lr => go(lr._1, !pol) -> go(lr._2, pol))
      () => {
        val newVars = ty.vars.flatMap { case tv => varSubst get tv match {
          case Some(Some(tv2)) => tv2 :: Nil
          case Some(None) => Nil
          case None => tv :: Nil
        }}
        CompactType(newVars, ty.prims,
          rec_.map(r => r.map(f => f._1 -> f._2())),
          fun_.map(lr => lr._1() -> lr._2()))
      }
    }
    val gone = go(cty.term, true)
    println(s"[occ] ${coOccurrences}")
    println(s"[rec] ${recVars}")
    
    // Simplify away those non-recursive variables that only occur in positive or negative positions:
    allVars.foreach { case v0 => if (!recVars.contains(v0)) {
      (coOccurrences.get(true -> v0), coOccurrences.get(false -> v0)) match {
        case (Some(_), None) | (None, Some(_)) =>
          println(s"[!] $v0")
          varSubst += v0 -> None; ()
        case occ => assert(occ =/= (None, None))
      }
    }}
    // Unify equivalent variables based on polar co-occurrence analysis:
    val pols = true :: false :: Nil
    allVars.foreach { case v => if (!varSubst.contains(v)) {
      println(s"[v] $v ${coOccurrences.get(true -> v)} ${coOccurrences.get(false -> v)}")
      pols.foreach { pol =>
        coOccurrences.get(pol -> v).iterator.flatMap(_.iterator).foreach {
          case w: Variable if !(w is v) && !varSubst.contains(w) && (recVars.contains(v) === recVars.contains(w)) =>
            // Note: We avoid merging rec and non-rec vars, because the non-rec one may not be strictly polar ^
            //       As an example of this, see [test:T1].
            println(s"[w] $w ${coOccurrences.get(pol -> w)}")
            if (coOccurrences.get(pol -> w).forall(_(v))) {
              println(s"[U] $w := $v") // we unify w into v
              varSubst += w -> Some(v)
              // Since w gets unified with v, we need to merge their bounds if they are recursive,
              // and otherwise merge the other co-occurrences of v and w from the other polarity (!pol).
              // For instance,
              //  consider that if we merge v and w in `(v & w) -> v & x -> w -> x`
              //  we get `v -> v & x -> v -> x`
              //  and the old positive co-occ of v, {v,x} should be changed to just {v,x} & {w,v} == {v}!
              recVars.get(w) match {
                case Some(b_w) => // `w` is a recursive variable, so `v` is too (see `recVars.contains` guard above)
                  assert(!coOccurrences.contains((!pol) -> w)) // recursive types have strict polarity
                  recVars -= w // w is merged into v, so we forget about it
                  val b_v = recVars(v) // `v` is recursive so `recVars(v)` is defined
                  // and record the new recursive bound for v:
                  recVars += v -> (() => CompactType.merge(pol)(b_v(), b_w()))
                case None => // `w` is NOT recursive
                  val wCoOcss = coOccurrences((!pol) -> w)
                  // ^ this must be defined otherwise we'd already have simplified away the non-rec variable
                  coOccurrences((!pol) -> v).filterInPlace(t => t === v || wCoOcss(t))
                  // ^ `w` is not recursive, so `v` is not either, and the same reasoning applies
              }
            }; ()
          case atom: Primitive if (coOccurrences.get(!pol -> v).exists(_(atom))) =>
            varSubst += v -> None; ()
          case _ =>
        }
      }
    }}
    println(s"[sub] ${varSubst.map(k => k._1.toString + " -> " + k._2).mkString(", ")}")
    
    CompactTypeScheme(gone(), recVars.view.mapValues(_()).toMap)
  }
  
  /** Coalesces a CompactTypeScheme into a Type while performing hash-consing
   * to tie recursive type knots a bit tighter, when possible. */
  def coalesceCompactType(cty: CompactTypeScheme): Type = {
    def go(ty: CompactTypeOrVariable, pol: Boolean)
          (implicit inProcess: Map[(CompactTypeOrVariable, Boolean), () => TypeVariable]): Type = {
      inProcess.get(ty -> pol) match { // Note: we use pat-mat instead of `foreach` to avoid non-local return
        case Some(t) =>
          val res = t()
          println(s"REC[$pol] $ty -> $res")
          return res
        case None => ()
      }
      var isRecursive = false
      lazy val v = {
        isRecursive = true
        (ty match {
          case tv: Variable => tv
          case _ => new Variable(0, Nil, Nil)
        }).asTypeVar
      }
      val res = inProcess + (ty -> pol -> (() => v)) pipe { implicit inProcess =>
        ty match {
          case tv: Variable => cty.recVars.get(tv).fold(tv.asTypeVar: Type)(go(_, pol))
          case CompactType(vars, prims, rec, fun) =>
            val (extr, mrg) = if (pol) (Bot, Union) else (Top, Inter)
            (vars.iterator.map(go(_, pol)).toList
              ::: prims.iterator.map(p => PrimitiveType(p.name)).toList
              ::: rec.map(fs => RecordType(fs.toList.map(f => f._1 -> go(f._2, pol)))).toList
              ::: fun.map(lr => FunctionType(go(lr._1, !pol), go(lr._2, pol))).toList
            ).reduceOption(mrg).getOrElse(extr)
        }
      }
      if (isRecursive) RecursiveType(v, res) else res
    }
    go(cty.term, true)(Map.empty)
  }
  
  
}
