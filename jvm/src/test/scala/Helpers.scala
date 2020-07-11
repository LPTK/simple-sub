package simplesub

object Helpers {
  
  implicit class TypeOps(ty: Type) {
    
    def showAsMLsub: String = {
      showAsMLsubIn(mkCtx(false), 0)
    }
    
    private def parensIf(str: String, cnd: Boolean): String = if (cnd) "(" + str + ")" else str
    def showAsMLsubIn(ctx: Map[TypeVar, String], outerPrec: Int): String = ty match {
      case Top => "Top"
      case Bot => "Bot"
      case Primitive(name) => name
      case uv: TypeVar => ctx(uv)
      case Recursive(n, b) => parensIf(s"rec ${ctx(n)} = ${b.showAsMLsubIn(ctx, 31)}", outerPrec > 10)
      case Function(l, r) => parensIf(l.showAsMLsubIn(ctx, 11) + " -> " + r.showAsMLsubIn(ctx, 10), outerPrec > 10)
      case Record(fs) => fs.map(nt => s"${nt._1}: ${nt._2.showAsMLsubIn(ctx, 0)}").mkString("{", "; ", "}")
      case Union(l, r) => parensIf(l.showAsMLsubIn(ctx, 20) + " | " + r.showAsMLsubIn(ctx, 20), outerPrec > 20)
      case Inter(l, r) => parensIf(l.showAsMLsubIn(ctx, 25) + " & " + r.showAsMLsubIn(ctx, 25), outerPrec > 25)
    }
    
    def mkCtx(addTicks: Boolean): Map[TypeVar, String] = {
      val vars = ty.typeVarsList.distinct
      vars.zipWithIndex.map {
        case (tv, idx) =>
          def nme = {
            // assert(idx <= 'z' - 'a', "TODO handle case of not enough chars")
            ('a' + idx).toChar.toString
          }
          tv -> (if (addTicks) "'" + nme else nme)
      }.toMap
    }
    
  }
  
  implicit class TermOps(trm: Term) {
    
    def freeVars: Set[String] = trm match {
      case Lit(value) => Set.empty
      case Var(name) => Set.empty[String] + name
      case Lam(name, rhs) => rhs.freeVars - name
      case App(lhs, rhs) => lhs.freeVars ++ rhs.freeVars
      case Rcd(fields) => fields.iterator.flatMap(_._2.freeVars).toSet
      case Sel(receiver, fieldName) => receiver.freeVars
      case Let(isRec, name, rhs, body) =>
        (body.freeVars - name) ++ (if (isRec) rhs.freeVars - name else rhs.freeVars)
    }
    
    def show: String = trm match {
      case Lit(value) => value.toString
      case Var(name) => name
      case Lam(name, rhs) => s"(fun $name -> ${rhs.show})"
      case App(lhs, rhs) => s"(${lhs.show} ${rhs.show})"
      case Rcd(fields) =>
        fields.iterator.map(nv => nv._1 + " = " + nv._2.show).mkString("{", "; ", "}")
      case Sel(receiver, fieldName) => receiver.show + "." + fieldName
      case Let(isRec, name, rhs, body) =>
        s"(let${if (isRec) " rec" else ""} $name = ${rhs.show} in ${body.show})"
    }
    
  }
  
}
