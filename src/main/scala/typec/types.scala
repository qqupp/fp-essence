package typec
import scala.language.higherKinds

trait Monad[M[_]] {
  def unitM[A](a: A): M[A]
  def bindM[A, B](ma: M[A])(f: A => M[B]): M[B]
}

trait Showable[T] {
  def showM(v: T): String
}

trait Errorable[M[_]] {
  def errorM[V](v: V, message: String): M[V]
}

trait Resettable[M[_]] {
  def resetM[V](p: Int)(m: M[V]): M[V]
}

trait Exposable[M[_]] {
  def expose[T](v: M[T]): Option[T]
}
