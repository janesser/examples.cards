package skat

import cards._

import skat.SkatDeckFactory._
import skat.SkatGameFactory._

trait SkatTestGames {
  self: org.scalatest.Matchers =>

  private def defaultGame = Grand(_)

  private def defaultBid(declarerCards: List[SkatCard]) = 18

  private def defaultSkat(declarerCards: List[SkatCard]) = (declarerCards.head, declarerCards.tail.head)

  protected def defendersGiveUp(game: Game): Game = game.schenken(game.players.tail)

  implicit def unwrapRunning(tg: TestGame): Game = tg.runningGame

  implicit def unwrapEvaluated(tg: TestGame): GameEvaluation = tg.evaluatedGame

  case class TestGame(declarerGame: List[SkatCard] => SkatGame = defaultGame,
                      declarerBid: List[SkatCard] => Int = defaultBid,
                      declarerSkat: List[SkatCard] => (SkatCard, SkatCard) = defaultSkat,
                      playGame: Game => Game = defendersGiveUp)(implicit players: List[Player]) {
    val deck = create()

    /**
      * distributed:
      * player 1 = clubs A-Q, spades (J)-8, hearts 7, diamonds A-K = declarer
      * player 2 = clubs (J)-9, spades 7, hearts A-Q, diamonds Q+(J)+10
      * player 3 = clubs 8+7, spades A, hearts (J)-8, diamonds 9-7
      * skat = spades K+Q
      */
    val bidding = {
      distribute(deck, players)
    }

    /**
      * declared game:
      * player 1 = clubs A-Q, spades (J)-8, hearts 7, diamonds A-K = declarer
      * player 2 = clubs (J)-9, spades 7, hearts A-Q, diamonds Q+(J)+10
      * player 3 = clubs 8+7, spades A, hearts (J)-8, diamonds 9-7
      * skat = spades K+Q
      */
    val runningGame = {
      val declarer = bidding.takeSkat(bidding.player1)

      val skat = (declarer.cards(0), declarer.cards(1))
      val game = bidding.completeBidding(declarer, declarerBid(declarer.cards), declarerGame(declarer.cards), skat)

      new org.scalatest.SpecLike {
        game.players(0).cards.head === SkatCard(Clubs, Ace)
        game.players(0).cards.last === SkatCard(Diamonds, King)
        game.players(1).cards.head === SkatCard(Clubs, Jack)
        game.players(1).cards.last === SkatCard(Diamonds, Ten)
        game.players(2).cards.head === SkatCard(Clubs, Eight)
        game.players(2).cards.last === SkatCard(Diamonds, Seven)

        game.skat === List(SkatCard(Spades, King), SkatCard(Spades, Queen))
      }

      game
    }

    /**
      * @see #playGame = defendersGiveUp
      */
    val evaluatedGame = {
      val game: Game = playGame(runningGame)

      game.evaluate()
    }
  }

}
