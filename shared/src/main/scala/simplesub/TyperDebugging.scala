package simplesub

import scala.collection.mutable.{Map => MutMap, Set => MutSet, LinkedHashMap, LinkedHashSet}
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.annotation.tailrec

/** Inessential methods used to help debugging. */
abstract class TyperDebugging { self: Typer =>
  
  // Shadow Predef functions with debugging-flag-enabled ones:
  def println(msg: => Any): Unit = if (dbg) scala.Predef.println(msg)
  def assert(assertion: => Boolean): Unit = if (dbg) scala.Predef.assert(assertion)
  
  trait SimpleTypeImpl { self: SimpleType =>
    
    def children: List[SimpleType] = this match {
      case tv: Variable => tv.lowerBounds ::: tv.upperBounds
      case Function(l, r) => l :: r :: Nil
      case Record(fs) => fs.map(_._2)
      case Primitive(_) => Nil
    }
    def getVars: Set[Variable] = {
      val res = MutSet.empty[Variable]
      @tailrec def rec(queue: List[SimpleType]): Unit = queue match {
        case (tv: Variable) :: tys =>
          if (res(tv)) rec(tys)
          else { res += tv; rec(tv.children ::: tys) }
        case ty :: tys => rec(ty.children ::: tys)
        case Nil => ()
      }
      rec(this :: Nil)
      SortedSet.from(res)(Ordering.by(_.uid))
    }
    def show: String = coalesceType(this).show
    def showBounds: String =
      getVars.iterator.filter(tv => (tv.upperBounds ++ tv.lowerBounds).nonEmpty).map(tv =>
        tv.toString
          + (if (tv.lowerBounds.isEmpty) "" else " :> " + tv.lowerBounds.mkString(" | "))
          + (if (tv.upperBounds.isEmpty) "" else " <: " + tv.upperBounds.mkString(" & "))
      ).mkString(", ")
    
  }
  
}
