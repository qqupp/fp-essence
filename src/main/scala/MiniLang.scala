import exprlang.AbstractSyntax._
import exprlang.Semantic._
import exprlang.SemanticDomain.Value
import typec._
import Terms._

object MiniLang extends App{

  def runTest[T[_]](title: String)(termList: List[Term])(implicit m: Monad[T],
                                                         s: Showable[T[Value]],
                                                         er: Errorable[T],
                                                         res: Resettable[T]) =
    println(
      (title :: termList.map( term => interpretTerm(term))).mkString("", "\n", "\n")
    )

  import instances.VariationTwo._
  runTest("Variation Two: Position => Error Monad")(testTerms ++ posTerms)

}
