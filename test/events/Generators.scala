package events

import org.scalacheck.Gen

object Generators {
  def interleaved[T](lists: List[T]*): Gen[List[T]] = {
    val nonEmptyLists = lists.filter(_.nonEmpty)
    if (nonEmptyLists.isEmpty) Gen.value(Nil)
    else for {
      choice <- Gen.choose(0, nonEmptyLists.size - 1)
      chosen = nonEmptyLists(choice)
      rest <- interleaved(nonEmptyLists.updated(choice, chosen.tail): _*)
    } yield {
      chosen.head :: rest
    }
  }
}
