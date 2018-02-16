import exprlang.abstractSyntax._
import exprlang.semanticDomain._
import exprlang.semantic._
import exprlang.environment
import typec._

object L extends App{

  val term0: Term = Appl(
    Lam("x", Add( Var("x"), Var("x")))
    ,Add(Con(10), Con(11))
  )

  {
    import instances.variationZero._
    println(test(term0))
  }
}
