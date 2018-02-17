package instances

import exprlang.SemanticDomain.Value
import typec.{Errorable, Monad, Resettable, Showable}


object VariationTwo {

  import instances.VariationOne._

  type PositionM[T] = Int => ErrorM[T]

  implicit val interpreterWithPosition = new Monad[PositionM] with Errorable[PositionM]
    with Showable[PositionM[Value]] with Resettable[PositionM]{

    val mErrM = implicitly[Monad[ErrorM]]
    val mErrE = implicitly[Errorable[ErrorM]]
    val mShow = implicitly[Showable[ErrorM[Value]]]

    override def unitM[A](a: A) =
      (_: Int) => mErrM.unitM(a)

    override def errorM[V](v: V, message: String) =
      (p: Int) => mErrE.errorM(v, s"position $p $message")

    override def bindM[A, B](ma: PositionM[A])(f: A => PositionM[B]) =
      (p: Int) => mErrM.bindM( ma(p) )( x => f(x)(p) )

    override def showM(v: PositionM[Value]) =
      mShow.showM(v(0))

    override def resetM[V](p: Int)(m: PositionM[V]) =
      (_: Int) => m(p)
  }
}
