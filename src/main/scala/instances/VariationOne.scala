package instances

import exprlang.SemanticDomain.Value
import typec.{Errorable, Monad, Showable}
import exprlang.Semantic._


object VariationOne {

  sealed trait ErrorM[T]
  case class Success[T](v: T) extends ErrorM[T]
  case class Error[T](e: String) extends ErrorM[T]

  implicit val interpreterWithErrors = new Monad[ErrorM] with Showable[ErrorM[Value]] with Errorable[ErrorM] {

    override def unitM[A](a: A) = Success(a)

    override def bindM[A, B](ma: ErrorM[A])(f: A => ErrorM[B]) = ma match {
      case Error(s) => Error(s)
      case Success(v) => f(v)
    }

    override def showM(v: ErrorM[Value]) = v match {
      case Error(s) => s"Error: $s"
      case Success(v) => s"Success ${showVal(v)}"
    }

    override def errorM[V](v: V, message: String) = Error(message)
  }
}
