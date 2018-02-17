package exprlang

import scala.language.higherKinds

object SemanticDomain {

  sealed trait Value
  case object Wrong extends Value
  case class Num(i: Int) extends Value
  case class Fun[M[_]](f: Value => M[Value]) extends Value

}
