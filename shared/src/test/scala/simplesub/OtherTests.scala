package simplesub

import org.scalatest.funsuite.AnyFunSuite

@SuppressWarnings(Array("org.wartremover.warts.Equals"))
class OtherTests extends AnyFunSuite {
  
  test("canonicalization produces LCD") {
    
    val typer = new Typer(false) with TypeSimplifier
    import typer.{assert => _, _}
    val tv0, tv1, tv3 = freshVar(0)
    
    // {f: {B: int, f: 'a}} as 'a  –  cycle length 2
    val st0 = Record("f"->Record("f"->tv0::"B"->IntType::Nil)::Nil)
    tv0.lowerBounds ::= st0
    
    // {f: {B: int, f: {A: int, f: 'a}}} as 'a  –  cycle length 3
    val st1 = Record("f"->Record("f"->Record("f"->tv1::"A"->IntType::Nil)::"B"->IntType::Nil)::Nil)
    tv1.lowerBounds ::= st1
    tv3.lowerBounds = tv0 :: tv1 :: Nil
    
    // println(tv3.showBounds)
    
    val ct = canonicalizeType(tv3)
    val sct = simplifyType(ct)
    val csct = coalesceCompactType(sct).show
    
    assert(csct == "{f: {B: int, f: {f: {f: {f: {f: 'a}}}}}} as 'a") // cycle length 6
    
  }
  
}
