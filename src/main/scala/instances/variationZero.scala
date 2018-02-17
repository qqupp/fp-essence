package instances

import typec.{Monad, Showable}
import exprlang.semanticDomain._
import exprlang.semantic._

object variationZero {

  case class Id[T](v: T)

  implicit val standardInterpreter = new Monad[Id] with Showable[Id[Value]] {
    override def unitM[T](a: T) = Id(a)

    override def bindM[A, B](ma: Id[A])(f: A => Id[B]) = f(ma.v)

    override def showM(v: Id[Value]) = showVal(v.v)
  }
}
