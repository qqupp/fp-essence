import exprlang.abstractSyntax._
import exprlang.semantic._

object MiniLang extends App{

  val term0: Term = Appl(
    Lam("x", Add( Var("x"), Var("x")))
    ,Add(Con(10), Con(11))
  )

  val term1: Term =
    Appl( Lam("x", Appl(Lam("y", Add( Var("y"), Var("x"))), Con(2))),
      Add(Con(10), Con(11))
    )

  val ifterm = IfzThenElse(Add(Con(1),Con(-1)), term0, term1)

  {
    import instances.variationZero._
    println(test(ifterm))
  }
}
