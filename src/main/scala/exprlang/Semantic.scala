package exprlang

import AbstractSyntax._
import SemanticDomain._
import Environment._
import typec._

import scala.language.higherKinds

object Semantic {

  def semAdd[M[_]](t1: Value, t2: Value)(implicit m: Monad[M],
                                         e: Errorable[M]): M[Value] =
    (t1, t2) match {
      case (Num(x), Num(y)) => m.unitM(Num(x + y))
      case _ => e.errorM(Wrong,
        s"should be numbers: ${List(t1,t2).map(showVal(_)).mkString(" ")}")
    }

  def semApply[M[_]](eFun: Value, eVal: Value)(implicit m: Monad[M],
                                               e: Errorable[M], expo: Exposable[M]): M[Value] =
    eFun match {
      case fu: Fun[M] => fu match { case Fun(f) => f(eVal) }
      case _ => e.errorM(Wrong, s"should be function: ${showVal(eFun)}")
    }

  def interpret[M[_]](t: Term)(e: Environment[M])(implicit m: Monad[M],
                                                  er: Errorable[M],
                                                  res: Resettable[M],
                                                  expose: Exposable[M]): M[Value] =
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
      case IfzThenElse(t0, t1, t2) => {
        val testN = expose.expose(interpret(t0)(e))
        if ( testN == Some(Num(0)))
          interpret(t1)(e)
        else
          interpret(t2)(e)
      }
      case At(p, t) => res.resetM(p)(interpret(t)(e))
    }

  def showVal(v: Value): String = v match {
    case Wrong => "<Wrong>"
    case Fun(x) => "<Function>"
    case Num(i) => i.toString
  }

  def interpretTerm[M[_]](t: Term)(implicit m: Monad[M], s: Showable[M[Value]],
                                   er: Errorable[M], res: Resettable[M], expo: Exposable[M]): String = {
    val mTerm: M[Value] = interpret(t)(emptyEnv[M])
    s.showM(mTerm)
  }

}
