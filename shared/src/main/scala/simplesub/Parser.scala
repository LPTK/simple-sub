package simplesub

import scala.util.chaining._
import fastparse._, fastparse.ScalaWhitespace._

@SuppressWarnings(Array("org.wartremover.warts.All"))
object Parser {
  
  val keywords = Set("let", "rec", "in", "fun", "if", "then", "else", "true", "false")
  def kw[X: P](s: String) = s ~~ !(letter | digit | "_" | "'")
  
  def letter[X: P]     = P( lowercase | uppercase )
  def lowercase[X: P]  = P( CharIn("a-z") )
  def uppercase[X: P]  = P( CharIn("A-Z") )
  def digit[X: P]      = P( CharIn("0-9") )
  def number[X: P]: P[Int] = P( CharIn("0-9").repX(1).!.map(_.toInt) )
  def ident[X: P]: P[String] =
    P( (letter | "_") ~~ (letter | digit | "_" | "'").repX ).!.filter(!keywords(_))
  
  def term[X: P]: P[Term] = P( let | fun | ite | apps )
  def const[X: P]: P[Term] = number.map(Lit)
  def variable[X: P]: P[Term] = (ident | "true".! | "false".!).map(Var)
  def parens[X: P]: P[Term] = P( "(" ~/ term ~ ")" )
  def subtermNoSel[X: P]: P[Term] = P( parens | record | const | variable )
  def subterm[X: P]: P[Term] = P( subtermNoSel ~ ("." ~/ ident).rep ).map {
    case (st, sels) => sels.foldLeft(st)(Sel) }
  def record[X: P]: P[Term] = P( "{" ~/ (ident ~ "=" ~ term).rep(sep = ";") ~ "}" )
    .filter(xs => xs.map(_._1).toSet.size === xs.size).map(_.toList pipe Rcd)
  def fun[X: P]: P[Term] = P( kw("fun") ~/ ident ~ "->" ~ term ).map(Lam.tupled)
  def let[X: P]: P[Term] =
    P( kw("let") ~/ kw("rec").!.?.map(_.isDefined) ~ ident ~ "=" ~ term ~ kw("in") ~ term )
    .map(Let.tupled)
  def ite[X: P]: P[Term] = P( kw("if") ~/ term ~ kw("then") ~ term ~ kw("else") ~ term ).map(ite =>
    App(App(App(Var("if"), ite._1), ite._2), ite._3))
  def apps[X: P]: P[Term] = P( subterm.rep(1).map(_.reduce(App)) )
  
  def expr[X: P]: P[Term] = P( term ~ End )
  
  def toplvl[X: P]: P[(Boolean, String, Term)] =
    P( kw("let") ~/ kw("rec").!.?.map(_.isDefined) ~ ident ~ "=" ~ term )
  def pgrm[X: P]: P[Pgrm] = P( ("" ~ toplvl).rep.map(_.toList) ~ End ).map(Pgrm)
  
}
