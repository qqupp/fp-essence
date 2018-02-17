package exprlang

object AbstractSyntax {

  type Name = String

  sealed trait Term
  case class Var(name: Name) extends Term
  case class Con(i: Int) extends Term
  case class Add(t1: Term, t2: Term) extends Term
  case class Lam(name: Name, t: Term) extends Term
  case class Appl(t1: Term, t2: Term) extends Term

  case class IfzThenElse(t0: Term, t1: Term, t2: Term) extends Term
}
