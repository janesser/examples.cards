package skat

import cards.SkatCardSuites._
import cards._
import org.scalatest.{FlatSpec, Matchers}
import skat.SkatDeckFactory._
import skat.SkatGameFactory._
import skat.SkatRules._

class SkatTests extends FlatSpec with Matchers with SkatTestGames {

  def validDeck(deck: List[SkatCard]): Unit = {
    allSuites foreach { suite =>
      deck.count(_.suite == suite) shouldEqual allCards.size
    }

    allCards foreach { card =>
      deck.count(_.card == card) shouldEqual allSuites.size
    }
  }

  "SkatDeckFactory" should "provide a french deck" in {
    validDeck(create())
  }

  it should "privide shuffled french deck" in {
    val deck1 = createShuffled()
    val deck2 = createShuffled()

    validDeck(deck1)
    validDeck(deck2)

    deck1 should not equal (deck2)
  }

  implicit val TEST_PLAYERS = List(Player("test-player-1"), Player("test-player-2"), Player("test-player-3"))

  "SkatGameFactory" should "prepare bidding" in {
    val bidding = distribute(create(), TEST_PLAYERS)
    val players = List(bidding.player1, bidding.player2, bidding.player3)

    players foreach { player =>
      player.cards.size shouldEqual PLAYER_CARDS
      // hand uniqueness
      players.filter(_ != player) foreach { other =>
        (player.cards -- other.cards).size shouldEqual PLAYER_CARDS
      }
    }
  }

  it should "refuse incomple decks" in {
    intercept[IllegalArgumentException] {
      distribute(create().tail, TEST_PLAYERS)
    }
  }

  it should "refuse less than three players" in {
    intercept[IllegalArgumentException] {
      distribute(create(), TEST_PLAYERS.tail)
    }
  }

  def testGrandGame(): Game = TestGame(Grand(_), _ => 18)

  "Bidding" should "completed should return Game" in testGrandGame

  it should "detect over-bidding" in {
    intercept[IllegalArgumentException] {
      TestGame(_ => Null, _ => SkatRules.NULL_SCORE + 1)
    }
  }

  it should "allow over-bidding up to SchneiderSchwartz" in {
    TestGame(Grand(_), _ => 3 * 24)

    intercept[IllegalArgumentException] {
      TestGame(Grand(_), _ => 3 * 24 + 1)
    }
  }

  "Game" should "take valid turn" in {
    implicit val game = testGrandGame()

    val opener = game.declarer
    val card1 = opener.cards.head // clubs A
    val player2 = game.players.tail.head
    val card2 = player2.cards.tail.head // clubs 10
    val player3 = game.players.tail.tail.head
    val card3 = player3.cards.head // clubs 8

    val nextGameTurn = SkatTrick(opener, card1, player2, card2, player3, card3)
    nextGameTurn.lastTurnPlayers.get.head === opener
  }

  it should "deny invalid turn" in {
    implicit val game = testGrandGame()

    val opener = game.declarer
    val card1 = opener.cards.last // diamonds K
    val player2 = game.players.tail.head
    val card2 = player2.cards.last // diamonds 10
    val player3 = game.players.tail.tail.head
    val card3 = player3.cards.head // clubs 8

    intercept[IllegalArgumentException] {
      SkatTrick(opener, card1, player2, card2, player3, card3)
    }
  }

  it should "should identify jack regardless of suite" in {
    implicit val game = testGrandGame()

    val opener = game.declarer
    val card1 = opener.cards.head // clubs A
    val player2 = game.players.tail.head
    val card2 = player2.cards.head // clubs J
    val player3 = game.players.tail.tail.head
    val card3 = player3.cards.head // clubs 8

    intercept[IllegalArgumentException] {
      SkatTrick(opener, card1, player2, card2, player3, card3)
    }
  }

  it should "allow schenken" in {
    val game = testGrandGame()

    game.schenken(game.declarer).evaluate().hasDeclarerWon === false
    game.schenken(game.defenders).evaluate().hasDeclarerWon === true
  }

  "Null" should "be lost on first trick disregarding score" in {
    Null.gameWon(List()) shouldEqual true
    Null.gameWon(List(SkatCard(Clubs, Seven), SkatCard(Clubs, Eight), SkatCard(Clubs, Nine))) shouldEqual false
  }

  "GameEvaluation" should "detect winner " in {
    val evaluation: GameEvaluation = TestGame(playGame = defendersGiveUp)

    evaluation.game.gameOver() shouldBe true
    evaluation.hasDeclarerWon shouldBe true
  }
}
