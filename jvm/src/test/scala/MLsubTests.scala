package simplesub

package mlsub

import scala.util.chaining._
import scala.collection.mutable.Buffer
import scala.util.Random

import Helpers._

// This program enumerates well-scoped terms
// and compares the results of Simple-sub and MLsub type inference on them, making sure that they agree.
object MLsubTests extends scala.App {{
  
  val seed = 42
  val rand = new Random(seed)
  
  // 'P' means the probability to generate each non-leaf subterm
  // StartP is the initial probability
  // MultP is the amount by which we multiply the current probability before recursing
  
  
  // Uncomment the latter settings for the full n = 1,313,832 tests
  
  // n == 799
  val StartP = 50
  val MultP = 0.11
  val fieldNames = "u" :: "v" :: Nil
  val varNames = "x" :: "y" :: "z" :: Nil
  
  // // n == 1313832
  // val StartP = 200
  // val MultP = 0.11
  // val fieldNames = "u" :: "v" :: "w" :: Nil
  // val varNames = "x" :: "y" :: "z" :: "s" :: "t" :: Nil
  
  
  // Enumerate expressions based on StartP and MultP
  def enumerate(emit: Term => Unit): Unit = {
    def rec(curP: Double)(emit: Term => Unit)(implicit ctx: (List[String], List[String])): Unit =
    if (rand.nextFloat() <= curP) {
      val newP = curP * MultP
      emit(Lit(0))
      ctx._1.foreach(n => emit(Var(n)))
      rec(newP)(f => rec(newP)(a => emit(App(f, a))))
      if (ctx._2.nonEmpty) {
        val vn = ctx._2.head
        val lctx = vn :: ctx._1
        val rctx = ctx._2.tail
        rec(newP) { b =>
          emit(Lam(vn, b))
          rec(newP) { v =>
            emit(Let(v.freeVars(vn), vn, v, b))
          }(lctx, rctx)
        }(lctx, rctx)
      }
      for (fn0 <- fieldNames) {
        rec(newP)(r => emit(Sel(r, fn0)))
        rec(newP) { v0 =>
          emit(Rcd((fn0, v0) :: Nil))
          for (fn1 <- fieldNames.tail) if (fn1 =/= fn0) {
            rec(newP)(v1 => emit(Rcd((fn0, v0) :: (fn1, v1) :: Nil)))
          }
        }
      }
    }
    rec(StartP)(emit)((Nil, varNames))
  }
  
  
  val prefix = "val res : "
  val typeErrStr = "<type error>"
  
  val mlsub = new MLsub
  val typer = new Typer(dbg = false) with TypeSimplifier
  var tested = 0
  var wellTyped, illTyped = 0
  var notWellTyped, notIllTyped, notSub, notSup = 0
  
  val out = new java.io.PrintWriter(s"test_results_$MultP-$StartP.txt")
  
  try enumerate(trm => {
    if (tested % 10000 === 0) println(s"Testing... (${tested}/n)")
    val str = trm.show
    out.println(s"==== ${tested} ====  $str")
    
    val ans = try {
      val ty = typer.inferType(trm)
      val cty = typer.compactType(ty)
      val sty = typer.simplifyType(cty)
      val ety = typer.expandCompactType(sty)
      ety.showAsMLsub
    } catch { case _: TypeError => typeErrStr }
    out.println("  Simpl:  " + ans)
    
    val q = "let res = " + str
    val mlsubAns =
      mlsub.query(q)
      .tap(a => assert(a.startsWith(prefix)))
      .pipe(_.stripPrefix(prefix))
    out.println("  MLsub:  " + mlsubAns)
    
    if (mlsubAns === typeErrStr) illTyped += 1
    else wellTyped += 1
    
    if (mlsubAns =/= ans) {
      if (ans === typeErrStr) {
        out.println("ERROR: NOT WELL-TYPED")
        notWellTyped += 1
      } else if (mlsubAns === typeErrStr) {
        out.println("ERROR: NOT ILL-TYPED")
        notIllTyped += 1
      } else {
        if (!mlsub.check_<:(ans, mlsubAns)) {
          out.println("ERROR: NOT <:")
          notSub += 1
        }
        if (!mlsub.check_<:(mlsubAns, ans)) {
          out.println("ERROR: NOT :>")
          notSup += 1
        }
      }
    }
    
    tested += 1
  }) catch {
    case e: Throwable =>
      try e.printStackTrace(out) finally throw e
  } finally out.close()
  
  
  println(s"Done testing.")
  
  println(s"Programs tested: n = ${tested}")
  println(s"Well-typed:    $wellTyped")
  println(s"Ill-typed:     $illTyped")
  
  val errNum = notWellTyped + notIllTyped + notSub + notSup
  println(s"Errors found:  $errNum")
  println(s"  (error: not well-typed) — $notWellTyped")
  println(s"  (error: not ill-typed)  — $notIllTyped")
  println(s"  (error: not <:)         — $notSub")
  println(s"  (error: not :>)         — $notSup")
  
}}

class MLsub {
  
  private val mlsub = os.proc("mlsub/main.native").spawn()
  
  // Start MLsub with the definition of the `succ` combinator:
  assert(query("let succ = fun x -> x + 1") === "val succ : (int -> int)")
  
  def query(str: String): String = {
    mlsub.stdin.writeLine(str)
    mlsub.stdin.flush()
    mlsub.stdout.readLine()
  }
  
  def check_<:(lhs: String, rhs: String): Boolean = {
    val check = s"check ($lhs) <: ($rhs)".replace(',', ';')
    val res = query(check)
    res.startsWith("[Y]") || {
      assert(res.startsWith("[N]"))
      false
    }
  }
  
}
