package exprlang

import semanticDomain._
import abstractSyntax._
import monad.Monad

object environment {
  type Environment[M[_]] = Name => M[Value]

  def emptyEnv[M](implicit m: Monad[M]): Environment[M] = x => m.unitM(Wrong)

  def lookup[M](name: Name)(env: Environment[M])(implicit m: Monad[M]) =
    m.unitM(env(name))

  def bind[M](name: Name)(value: Value)(environment: Environment[M])(
      implicit m: Monad[M]): Environment =
    x => if (x == name) m.unitM(value) else environment(x)
}
