package exprlang

object semanticDomain {

  sealed trait Value
  case object Wrong extends Value
  case class Num(i: Int) extends Value
  case class Fun[M[_]](f: Value => M[Value])

}
