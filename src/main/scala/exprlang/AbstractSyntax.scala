package exprlang

object AbstractSyntax {

  type Name = String

  sealed trait Term {
    override def toString: String = this match {
      case Var(n) => n
      case Con(i) => i.toString
      case Add(t1, t2) => s"(${t1.toString} + ${t2.toString})"
      case Lam(n, t) => s"λ$n.${t.toString}"
      case Appl(t1, t2) => s"(${t1.toString})(${t2.toString})"
      case IfzThenElse(t0, t1, t2) => s"if (${t0.toString} == 0) then (${t1.toString}) else (${t2.toString})"
      case At(p, t) => s"""\nLine $p: ${t.toString}"""
    }
  }

  case class Var(name: Name) extends Term
  case class Con(i: Int) extends Term
  case class Add(t1: Term, t2: Term) extends Term
  case class Lam(name: Name, t: Term) extends Term
  case class Appl(t1: Term, t2: Term) extends Term
  case class IfzThenElse(t0: Term, t1: Term, t2: Term) extends Term
  case class At(pos: Int, t: Term) extends Term
}
