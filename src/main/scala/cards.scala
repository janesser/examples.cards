package object cards {

  trait CardSuite

  trait FrenchCardSuite extends CardSuite

  sealed abstract class SkatCardSuite(val score: Int) extends FrenchCardSuite

  case object Clubs extends SkatCardSuite(12)

  case object Spades extends SkatCardSuite(11)

  case object Hearts extends SkatCardSuite(10)

  case object Diamonds extends SkatCardSuite(9)


  trait SuiteCard

  sealed abstract class SkatSuiteCard(val score: Int) extends SuiteCard

  case object Ace extends SkatSuiteCard(11)

  case object King extends SkatSuiteCard(4)

  case object Queen extends SkatSuiteCard(3)

  case object Jack extends SkatSuiteCard(2)

  case object Ten extends SkatSuiteCard(10)

  case object Nine extends SkatSuiteCard(0)

  case object Eight extends SkatSuiteCard(0)

  case object Seven extends SkatSuiteCard(0)

  case object Six extends SuiteCard

  case object Five extends SuiteCard

  case object Four extends SuiteCard

  case object Three extends SuiteCard

  case object Two extends SuiteCard

  object FrenchCardSuites {
    val allSuites = List(Clubs, Spades, Hearts, Diamonds)
    val allCards = List(Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two)
  }

  object SkatCardSuites {
    val allSuites = List(Clubs, Spades, Hearts, Diamonds)
    val allCards = List(Ace, King, Queen, Jack, Ten, Nine, Eight, Seven)
  }


  object FrenchCardSuitesOrdering extends Ordering[FrenchCardSuite] {

    import FrenchCardSuites._
    import scala.collection.SortedSet

    val suitesCount = allSuites.size

    def ordinal(suite: FrenchCardSuite): Int =
      allSuites.indexOf(suite)

    override def compare(suite1: FrenchCardSuite, suite2: FrenchCardSuite): Int =
      ordinal(suite2) - ordinal(suite1)

    def toSortedSet(suites: Iterable[FrenchCardSuite]): SortedSet[FrenchCardSuite] =
      (SortedSet.newBuilder(this) ++= suites).result()
  }

  case class SkatCard(suiteCard: (FrenchCardSuite, SkatSuiteCard)) extends AnyVal {
    def suite: FrenchCardSuite = suiteCard._1

    def card: SkatSuiteCard = suiteCard._2
  }

}

