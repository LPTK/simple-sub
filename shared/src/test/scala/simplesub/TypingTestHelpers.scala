package simplesub

import org.scalatest._
import fastparse._
import Parser.expr
import fastparse.Parsed.Failure
import fastparse.Parsed.Success
import sourcecode.Line
import org.scalatest.funsuite.AnyFunSuite

@SuppressWarnings(Array("org.wartremover.warts.Equals"))
class TypingTestHelpers extends AnyFunSuite {
  
  def doTest(str: String, expected: String = "")(implicit line: Line): Unit = {
    val dbg = expected.isEmpty
    
    if (dbg) println(s">>> $str")
    val Success(term, index) = parse(str, expr(_), verboseFailures = true)
    
    val typer = new Typer(dbg) with TypeSimplifier
    val tyv = typer.inferType(term)
    
    if (dbg) {
      println("inferred: " + tyv)
      println(" where " + tyv.showBounds)
    }
    val cty = typer.canonicalizeType(tyv)
    if (dbg) println("compacted: " + cty)
    val sty = typer.simplifyType(cty)
    if (dbg) println("simplified: " + sty)
    val ety = typer.coalesceCompactType(sty)
    if (dbg) {
      println("coalesced raw: " + ety)
      println("coalesced: " + ety.show)
    }
    
    val res = ety.show
    if (dbg) {
      println("typed: " + res)
      println("---")
    } else {
      assert(res == expected, "at line " + line.value); ()
    }
  }
  def error(str: String, msg: String): Unit = {
    assert(intercept[TypeError](doTest(str, "<none>")).msg == msg); ()
  }
  
}
