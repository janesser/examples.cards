package skat.ai

import cards._
import org.scalatest.{FlatSpec, Matchers}
import skat.SkatDeckFactory._
import skat.SkatGameFactory._
import skat._

class RandomPlayerTest extends FlatSpec with Matchers with SkatTestGames {

  implicit val TEST_PLAYERS = List("random1", "random2", "random3").map(ComputerPlayer.random(_))

  implicit def randomPlayer(player: Player): RandomPlayer = player.asInstanceOf[RandomPlayer]

  "RandomPlayer" should "play card of his hand" in {
    val game: Game = TestGame()

    val declarer = game.players.head.asInstanceOf[RandomPlayer]
    declarer.cards should contain(declarer.nextCard())
  }

  it should "play until gameOver" in {
    implicit var game: Game = TestGame()


    while (!game.gameOver())
      game.lastTurnPlayers match {
        case Some(opener :: follower1 :: follower2 :: xs) =>
          val openerCard = opener.nextCard()

          val skatTrick = SkatTrick(opener, openerCard)

          game = {
            skatTrick.play(follower1, follower1.nextCard(skatTrick)) match {
              case Left(withFollower1) =>
                withFollower1.play(follower2, follower2.nextCard(skatTrick)) match {
                  case Right(g) => g
                }
            }
          }
      }

    game.gameOver() shouldBe true
  }

}
