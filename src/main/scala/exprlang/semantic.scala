package exprlang

import abstractSyntax._
import semanticDomain._
import environment._
import typec.{Monad, Showable}

object semantic {

  def semAdd[M[_]](t1: Value, t2: Value)(implicit m: Monad[M]): M[Value] = (t1, t2) match {
    case (Num(x), Num(y)) => m.unitM(Num(x + y))
    case _ => m.unitM(Wrong)
  }

  def semApply[M[_]](eFun: Value, eVal: Value)(implicit m: Monad[M]): M[Value] = (eFun, eVal) match {
    case (Fun(f),v) => f(v).asInstanceOf[M[Value]] // to fix the conversion  it returns a Any type
    case _ => m.unitM(Wrong)
  }

  def interpret[M[_]](t: Term)(e: Environment[M])(implicit m: Monad[M]): M[Value] =
    t match {
      case Var(name) => lookup(name)(e)
      case Con(int) => m.unitM(Num(int))
      case Add(t1, t2) =>
        m.bindM( interpret(t1)(e) ) { eT1 =>
          m.bindM( interpret(t2)(e) ) { eT2 =>
            semAdd(eT1, eT2)}
        }
      case Lam(name, term) =>
        m.unitM {
          Fun(
            (v: Value) => interpret(term)(bind(name)(v)(e)))
        }
      case Appl(t1,t2) =>
        m.bindM( interpret(t1)(e) ) { eFun =>
          m.bindM( interpret(t2)(e) ) { eVal =>
            semApply(eFun, eVal)
          }
        }
      case IfzThenElse(t0, t1, t2) =>
        if (interpret(t0)(e) == m.unitM(Num(0)))
          interpret(t1)(e)
        else
          interpret(t2)(e)
    }

  def showVal(v: Value): String = v match {
    case Wrong => "<Wrong>"
    case Fun(x) => "<Function>"
    case Num(i) => i.toString
  }

  def test[M[_]](t: Term)(implicit m: Monad[M], s: Showable[M[Value]]): String = {
    val mTerm: M[Value] = interpret(t)(emptyEnv[M])
    s.showM(mTerm)
  }

}
