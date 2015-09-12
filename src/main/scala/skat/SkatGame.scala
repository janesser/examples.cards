package skat

import cards._

sealed trait SkatGame {
  def score: Int

  def isTrump(card: SkatCard): Boolean

  protected trait SkatCardOrdering extends Ordering[SkatCard] {
    def cardsOrdered: List[SkatSuiteCard]

    def ordinal(c: SkatCard) = cardsOrdered.indexOf(c.card)

    def compare(x: SkatCard, y: SkatCard) =
      ordinal(y) - ordinal(x)
  }

  def cardOrdering: Ordering[SkatCard]

  def gameWon(declarerCards: List[SkatCard]): Boolean
}

object JacksGame {

  sealed trait Modifiers {
    def jacksOffset: Int

    def scoreToWin: Int
  }

  object Schneider extends Modifiers {
    override val jacksOffset: Int = 1

    override val scoreToWin = SkatRules.SCHNEIDER_GAMEVALUE_TO_WIN
  }

  object SchneiderSchwartz extends Modifiers {
    override val jacksOffset: Int = 2

    override val scoreToWin = SkatRules.SCHNEIDERSCHWARTZ_GAMEVALUE_TO_WIN
  }

}

abstract class JacksGame(declarerCards: List[SkatCard], modifiers: Option[JacksGame.Modifiers] = None) extends SkatGame {

  import FrenchCardSuitesOrdering.toSortedSet

  protected def isJack(c: SkatCard) = c.card == Jack

  protected def jacks = toSortedSet(declarerCards.filter(isJack).map(_.suite)).toList

  private val jacksOffset = modifiers match {
    case Some(m) => m.jacksOffset
    case _ => 0
  }

  private val jacksCount = jacks match {
    case (Clubs :: xs) =>
      xs match {
        case (Spades :: xss) =>
          xss match {
            case (Hearts :: xsss) =>
              xsss match {
                case (Diamonds :: xssss) => SkatRules.FOUR_JACKS
                case _ => SkatRules.THREE_JACKS
              }
            case _ => SkatRules.TWO_JACKS
          }
        case _ => SkatRules.ONE_JACK
      }
    // without Clubs Jack
    case (Spades :: xs) => SkatRules.WITHOUT_ONE_JACK
    case (Hearts :: xs) => SkatRules.WITHOUT_TWO_JACKS
    case (Diamonds :: xs) => SkatRules.WITHOUT_THREE_JACKS
    // without any Jack
    case _ => SkatRules.WITHOUT_ANY_JACKS
  }

  val jacksCounter: Int = jacksCount + jacksOffset

  val cardOrdering = new SkatCardOrdering {
    val cardsOrdered = List(Jack, Ace, Queen, Ten, Nine, Eight, Seven)

    override def compare(x: SkatCard, y: SkatCard): Int =
      if (isTrump(x) && isTrump(y))
        FrenchCardSuitesOrdering.compare(x.suite, y.suite)
      else
        super.compare(x, y)
  }

  override def gameWon(declarerCards: List[SkatCard]): Boolean = {
    val declarerScore: Int = declarerCards.foldLeft(0) {
      (acc, c) =>
        acc + c.card.score
    }

    val scoreToWin = modifiers match {
      case None => SkatRules.DEFAULT_GAMEVALUE_TO_WIN
      case Some(s) => s.scoreToWin
    }

    declarerScore > scoreToWin
  }
}

sealed trait HandGame

trait JacksHandGame extends HandGame {
  self: JacksGame =>

  override val jacksCounter: Int = self.jacksCounter + 1
}

class Suite(cards: List[SkatCard], suite: SkatCardSuite) extends JacksGame(cards) {
  def score = jacksCounter * suite.score

  def isTrump(c: SkatCard) = isJack(c) || c.suite == suite
}

class SuiteHand(cards: List[SkatCard], suite: SkatCardSuite) extends Suite(cards, suite) with JacksHandGame

object Grand {
  def apply(cards: List[SkatCard]) = new Grand(cards)

  def hand(cards: List[SkatCard]) = new GrandHand(cards)
}

class Grand(cards: List[SkatCard]) extends JacksGame(cards) {
  require(cards.size == SkatRules.PLAYER_CARDS + SkatRules.SKAT_CARDS)

  def score = jacksCounter * SkatRules.GRAND_SCORE

  def isTrump(c: SkatCard) = isJack(c)
}

class GrandHand(cards: List[SkatCard]) extends Grand(cards) with JacksHandGame

sealed trait NullGame extends SkatGame {
  override def isTrump(c: SkatCard) = false

  override val cardOrdering = new SkatCardOrdering {
    val cardsOrdered = List(Ace, King, Queen, Jack, Ten, Nine, Eight, Seven)
  }

  override def gameWon(declarerCards: List[SkatCard]): Boolean = declarerCards.isEmpty
}

case object Null extends NullGame {
  override val score = SkatRules.NULL_SCORE
}

case object NullHand extends NullGame with HandGame {
  override val score = SkatRules.NULL_HAND_SCORE
}

case object NullOuvert extends NullGame {
  override val score = SkatRules.NULL_OUVERT_SCORE
}

case object NullOuvertHand extends NullGame with HandGame {
  override val score = SkatRules.NULL_OUVERT_HAND_SCORE
}