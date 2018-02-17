package instances

import typec.{Errorable, Monad, Showable}
import exprlang.SemanticDomain._
import exprlang.Semantic._

object VariationZero {

  case class Id[T](v: T)

  implicit val standardInterpreter = new Monad[Id] with Showable[Id[Value]] with Errorable[Id] {
    override def unitM[T](a: T) = Id(a)

    override def bindM[A, B](ma: Id[A])(f: A => Id[B]) = f(ma.v)

    override def showM(v: Id[Value]) = showVal(v.v)

    override def errorM[V](v: V, message: String) = Id(v)
  }
}
