import exprlang.AbstractSyntax._
import exprlang.Semantic._
import exprlang.SemanticDomain.Value
import typec._

object MiniLang extends App{

  val term0: Term = Appl(
    Lam("x", Add( Var("x"), Var("x")))
    ,Add(Con(10), Con(11))
  )

  val term1: Term =
    Appl( Lam("x", Appl(Lam("y", Add( Var("y"), Var("x"))), Con(2))),
      Add(Con(10), Con(11))
    )

  val ifTerm = IfzThenElse(Add(Con(1),Con(-1)), term0, term1)

  val wrongTerm0 = Appl(Con(10),Con(2))
  val wrongTerm1 = Var("x")
  val wrongTerm2 = Add(Lam("y", Var("y")), Con(0))

  val testTerms = List(term0, term1, ifTerm,wrongTerm0, wrongTerm1, wrongTerm2)

  def runTest[T[_]](title: String)(implicit m: Monad[T],
                                   s: Showable[T[Value]], er: Errorable[T]) =
    println(
      (title :: testTerms.map( term => test(term))).mkString("", "\n", "\n")
    )

  {
    import instances.VariationZero._
    runTest("Variation Zero: Id Monad")
  }

  {
    import instances.VariationOne._
    runTest("Variation One: Error Monad")
  }

}
