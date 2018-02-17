package exprlang

import SemanticDomain._
import AbstractSyntax._
import typec.{Errorable, Monad}
import scala.language.higherKinds

object Environment {
  type Environment[M[_]] = Name => M[Value]

  def emptyEnv[M[_]](implicit m: Monad[M], e: Errorable[M]): Environment[M] =
    x => e.errorM(Wrong, s"unbound variable: $x")

  def lookup[M[_]](name: Name)(env: Environment[M])(implicit m: Monad[M]) =
    env(name)

  def bind[M[_]](name: Name)(value: Value)(environment: Environment[M])(
      implicit m: Monad[M]): Environment[M] =
    x => if (x == name) m.unitM(value) else environment(x)
}
