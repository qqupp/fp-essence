package exprlang

import semanticDomain._
import abstractSyntax._
import typec.Monad

object environment {
  type Environment[M[_]] = Name => M[Value]

  def emptyEnv[M[_]](implicit m: Monad[M]): Environment[M] =
    x => m.unitM(Wrong)

  def lookup[M[_]](name: Name)(env: Environment[M])(implicit m: Monad[M]) =
    env(name)

  def bind[M[_]](name: Name)(value: Value)(environment: Environment[M])(
      implicit m: Monad[M]): Environment[M] =
    x => if (x == name) m.unitM(value) else environment(x)
}
