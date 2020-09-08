package simplesub

import org.scalatest._
import fastparse._
import Parser.pgrm
import fastparse.Parsed.Failure
import fastparse.Parsed.Success
import sourcecode.Line
import org.scalatest.funsuite.AnyFunSuite

@SuppressWarnings(Array("org.wartremover.warts.Equals"))
class ProgramTests extends AnyFunSuite {
  
  implicit class ExpectedStr(val str: String)(implicit val line: Line)
  
  def doTest(str: String)(expected: ExpectedStr*): Unit = {
    val dbg = expected.exists(_.str.isEmpty)
    val Success(p, index) = parse(str, pgrm(_), verboseFailures = true)
    val typer = new Typer(dbg) with TypeSimplifier
    val tys = typer.inferTypes(p)
    (p.defs lazyZip tys lazyZip expected).foreach { (str, pty, exp) =>
      if (exp.str.isEmpty) println(s">>> $str")
      val ty = pty.fold(err => throw err, _.instantiate(0))
      val cty = typer.compactType(ty)
      val sty = typer.simplifyType(cty)
      val res = typer.coalesceCompactType(sty).show
      if (exp.str.nonEmpty) { assert(res == exp.str, "at line " + exp.line.value); () }
      else {
        println("inferred: " + ty)
        println(" where " + ty.showBounds)
        println(res)
        println("---")
      }
    }
    assert(tys.size == expected.size); ()
  }
  
  test("mlsub") { // from https://www.cl.cam.ac.uk/~sd601/mlsub/
    doTest("""
      let id = fun x -> x
      let twice = fun f -> fun x -> f (f x)
      let object1 = { x = 42; y = id }
      let object2 = { x = 17; y = false }
      let pick_an_object = fun b ->
        if b then object1 else object2
      let rec recursive_monster = fun x ->
        { thing = x;
          self = recursive_monster x }
    """)(
      "'a -> 'a",
      "('a ∨ 'b -> 'a) -> 'b -> 'a",
      "{x: int, y: 'a -> 'a}",
      "{x: int, y: bool}",
      "bool -> {x: int, y: bool ∨ ('a -> 'a)}",
      "'a -> {self: 'b, thing: 'a} as 'b",
    )
  }
  
  test("top-level-polymorphism") {
    doTest("""
      let id = fun x -> x
      let ab = {u = id 0; v = id true}
    """)(
      "'a -> 'a",
      "{u: int, v: bool}",
    )
  }
  
  test("rec-producer-consumer") {
    doTest("""
      let rec produce = fun arg -> { head = arg; tail = produce (succ arg) }
      let rec consume = fun strm -> add strm.head (consume strm.tail)
      
      let codata = produce 42
      let res = consume codata
      
      let rec codata2 = { head = 0; tail = { head = 1; tail = codata2 } }
      let res = consume codata2
      
      let rec produce3 = fun b -> { head = 123; tail = if b then codata else codata2 }
      let res = fun x -> consume (produce3 x)
      
      let consume2 =
        let rec go = fun strm -> add strm.head (add strm.tail.head (go strm.tail.tail))
        in fun strm -> add strm.head (go strm.tail)
        // in go
      // let rec consume2 = fun strm -> add strm.head (add strm.tail.head (consume2 strm.tail.tail))
      let res = consume2 codata2
    """)(
      "int -> {head: int, tail: 'a} as 'a",
      "{head: int, tail: 'a} as 'a -> int",
      "{head: int, tail: 'a} as 'a",
      "int",
      "{head: int, tail: {head: int, tail: 'a}} as 'a",
      "int",
      "bool -> {head: int, tail: {head: int, tail: {head: int, tail: 'a} as 'a " +
        "∨ {head: int, tail: {head: int, tail: {head: int, tail: 'b}} as 'b}}}",
        // ^ simplifying this would probably require more advanced
        // automata-based techniques such as the one proposed by Dolan
      "bool -> int",
      "{head: int, tail: {head: int, tail: 'a}} as 'a -> int",
      "int",
    )
  }
  
  test("misc") {
    doTest("""
      // 
      // From a comment on the blog post:
      // 
      let rec r = fun a -> r
      let join = fun a -> fun b -> if true then a else b
      let s = join r r
      // 
      // Inspired by [Pottier 98, chap 13.4]
      // 
      let rec f = fun x -> fun y -> add (f x.tail y) (f x y)
      let rec f = fun x -> fun y -> add (f x.tail y) (f y x)
      let rec f = fun x -> fun y -> add (f x.tail y) (f x y.tail)
      let rec f = fun x -> fun y -> add (f x.tail y.tail) (f x.tail y.tail)
      let rec f = fun x -> fun y -> add (f x.tail x.tail) (f y.tail y.tail)
      let rec f = fun x -> fun y -> add (f x.tail x) (f y.tail y)
      let rec f = fun x -> fun y -> add (f x.tail y) (f y.tail x)
      // 
      let f = fun x -> fun y -> if true then { l = x; r = y } else { l = y; r = x } // 2-crown
      // 
      // Inspired by [Pottier 98, chap 13.5]
      // 
      let rec f = fun x -> fun y -> if true then x else { t = f x.t y.t }
    """)(
      "(⊤ -> 'a) as 'a",
      "'a -> 'a -> 'a",
      "⊤ -> (⊤ -> 'a) as 'a ∨ (⊤ -> 'b) as 'b", // could simplify more
      
      "{tail: 'a} as 'a -> ⊤ -> int",
      "{tail: 'a} as 'a -> {tail: 'b} as 'b -> int",
      "{tail: 'a} as 'a -> {tail: 'b} as 'b -> int",
      "{tail: 'a} as 'a -> {tail: 'b} as 'b -> int",
      "{tail: 'b ∧ 'a} as 'b as 'a -> {tail: 'c ∧ 'd} as 'd as 'c -> int", // could simplify more (double rec type)
      // ^ MLsub says:
      //    let rec f = fun x -> fun y -> (f x.tail x) + (f y.tail y)
      //    val f : ({tail : (rec b = {tail : b})} -> ({tail : {tail : (rec a = {tail : a})}} -> int))
      "({tail: 'a} ∧ {tail: 'a}) as 'a -> {tail: ('b ∧ {tail: 'c}) as 'c} as 'b -> int", // could simplify more (the {tail: 'a} ∧ {tail: 'a})
      // ^ MLsub says:
      //    let rec f = fun x -> fun y -> (f x.tail x.tail) + (f y.tail y.tail)
      //    val f : ({tail : {tail : (rec b = {tail : b})}} -> ({tail : {tail : (rec a = {tail : a})}} -> int))
      "({tail: 'a} ∧ {tail: 'a}) as 'a -> {tail: ('b ∧ {tail: 'c}) as 'c} as 'b -> int",
      
      "'a -> 'a -> {l: 'a, r: 'a}",
      
      "('b ∧ {t: 'a}) as 'a -> {t: 'c} as 'c -> ('b ∨ {t: 'd}) as 'd",
      // ^ MLsub says:
      //    let rec f = fun x -> fun y -> if true then x else { t = f x.t y.t }
      //    val f : (({t : (rec d = ({t : d} & a))} & a) -> ({t : (rec c = {t : c})} -> ({t : (rec b = ({t : b} | a))} | a)))
      // ^ Pottier says a simplified version would essentially be, once translated to MLsub types:
      //    {t: 'a} as 'a -> 'a -> {t: 'd} as 'd
      // but even he does not infer that.
      // Notice the loss of connection between the first parameetr and the result, in his proposed type,
      // which he says is not necessary as it is actually implied.
      // He argues that if 'a <: F 'a and F 'b <: 'b then 'a <: 'b, for a type operator F,
      // which does indeed seem true (even in MLsub),
      // though leveraging such facts for simplification would require much more advanced reasoning.
    )
  }
  
}
