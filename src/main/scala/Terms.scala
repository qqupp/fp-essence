import exprlang.AbstractSyntax._

object Terms {

  val term0: Term = Appl(
    Lam("x", Add( Var("x"), Var("x")))
    ,Add(Con(10), Con(11))
  )

  val term1: Term =
    Appl( Lam("x", Appl(Lam("y", Add( Var("y"), Var("x"))), Con(2))),
      Add(Con(10), Con(11))
    )

  val ifTerm = IfzThenElse(Add(Con(2),Con(1)), term0, term1)

  val wrongTerm0 = Appl(Con(10),Con(2))
  val wrongTerm1 = Var("x")
  val wrongTerm2 = Add(Lam("y", Var("y")), Con(0))

  val posWrong0 = At(10, Appl( Lam("x", At(12, Appl(Lam("z", Add( Var("y"), Var("x"))), Con(2)))),
    Add(Con(10), Con(11))
  ))
  val posWrong1 = At(100, wrongTerm0)

  val testTerms = List(term0, term1, ifTerm,wrongTerm0, wrongTerm1, wrongTerm2)
  val posTerms = List(posWrong0, posWrong1)

}
