package simplesub


// Terms

final case class Pgrm(defs: List[(Boolean, String, Term)])

sealed abstract class Term
final case class Lit(value: Int)                                          extends Term
final case class Var(name: String)                                        extends Term
final case class Lam(name: String, rhs: Term)                             extends Term
final case class App(lhs: Term, rhs: Term)                                extends Term
final case class Rcd(fields: List[(String, Term)])                        extends Term
final case class Sel(receiver: Term, fieldName: String)                   extends Term
final case class Let(isRec: Boolean, name: String, rhs: Term, body: Term) extends Term


// Types

sealed abstract class Type extends TypeImpl
case object Top                                               extends Type
case object Bot                                               extends Type
final case class Union(lhs: Type, rhs: Type)                  extends Type
final case class Inter(lhs: Type, rhs: Type)                  extends Type
final case class FunctionType(lhs: Type, rhs: Type)           extends Type
final case class RecordType(fields: List[(String, Type)])     extends Type
final case class RecursiveType(uv: TypeVariable, body: Type)  extends Type
final case class PrimitiveType(name: String)                  extends Type
final class TypeVariable(val nameHint: String, val hash: Int) extends Type {
  override def toString: String = s"$nameHint:$hash"
}

