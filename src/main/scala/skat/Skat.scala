package skat

import cards._

object SkatRules {
  val DECLARER_COUNT = 1
  val DEFENDER_COUNT = 2

  val PLAYER_COUNT = DECLARER_COUNT + DEFENDER_COUNT
  val PLAYER_CARDS = 10

  def TRICKS_TO_PLAY = PLAYER_CARDS

  val SKAT_CARDS = 2

  val DEFAULT_GAMEVALUE_TO_WIN = 60
  val SCHNEIDER_GAMEVALUE_TO_WIN = 90
  val SCHNEIDERSCHWARTZ_GAMEVALUE_TO_WIN = 120

  val LOST_MULTIPLIER = 2

  val ONE_JACK = 1
  val WITHOUT_ONE_JACK = 1
  val TWO_JACKS = 2
  val WITHOUT_TWO_JACKS = 2
  val THREE_JACKS = 3
  val WITHOUT_THREE_JACKS = 3
  val FOUR_JACKS = 4
  val WITHOUT_ANY_JACKS = 4

  val GRAND_SCORE = 24
  val NULL_SCORE = 23
  val NULL_HAND_SCORE = 35
  val NULL_OUVERT_SCORE = 46
  val NULL_OUVERT_HAND_SCORE = 59

}

object SkatDeckFactory {

  import SkatCardSuites._

  val r = new scala.util.Random(1L)

  def create(): List[SkatCard] = {
    val suiteCards = allSuites map {
      suite =>
        allCards map {
          card => SkatCard(suite, card)
        }
    }
    suiteCards.flatten.toList
  }

  def createShuffled() = r.shuffle(create())
}
