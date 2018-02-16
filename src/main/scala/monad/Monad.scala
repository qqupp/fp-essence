package monad

trait Monad[M[_]] {
  def unitM[A](a: A): M[A]
  def bindM[A, B](ma: M[A])(f: A => M[B]): M[B]
  def showM[A](ma: M[A]): String

}
