package skat.ai

import java.util.Random

import cards._
import skat.SkatGameFactory._

object ComputerPlayer {
  def random(id: String): RandomPlayer = new RandomPlayer(id)

  def random(id: String, cards: List[SkatCard], r: Random): RandomPlayer = new RandomPlayer(id, cards, r)
}

trait ComputerPlayer {
  def nextCard(): SkatCard

  def nextCard(t: SkatTrick): SkatCard
}

class RandomPlayer(id: String, cards: List[SkatCard] = List(), r: Random = new Random(1L)) extends Player(id, cards) with ComputerPlayer {
  override def withCards(cards: List[SkatCard]): Player = new RandomPlayer(id, cards, r)

  def nextCard(): SkatCard = {
    cards.apply(r.nextInt(cards.size))
  }

  def nextCard(t: SkatTrick): SkatCard = {
    cards.find(t.matchSuite(_)) match {
      case Some(c) => c
      case None => nextCard()
    }
  }
}


