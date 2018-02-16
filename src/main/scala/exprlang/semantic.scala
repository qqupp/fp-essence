package exprlang

import abstractSyntax._
import semanticDomain._
import environment._
import monad.Monad

object semantic {

  def add[M[_]](t1: Value, t2: Value)(implicit m: Monad[M]): M[Value] = (t1, t2) match {
    case (Num(x), Num(y)) => m.unitM(Num(x + y))
    case _ => m.unitM(Wrong)
  }

  def interpret[M[_]](t: Term)(e: Environment[M])(implicit m: Monad[M]): M[Value] =
    t match {
      case Var(name) => lookup(name)(e)
      case Con(int) => m.unitM(Num(int))
      case Add(t1, t2) =>
        m.bindM( interpret(t1)(e) ) { eT1 =>
          m.bindM( interpret(t2)(e) ) { eT2 => add(eT1, eT2)}
        }
      case Lam(name, term) =>
        m.unitM {
          Fun(
            (v: Value) => interpret(term)(bind(name)(v)(e)))
        }
      case _ => ???
    }


}
