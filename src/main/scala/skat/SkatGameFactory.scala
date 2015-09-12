package skat

import cards._

object SkatGameFactory {

  type Skat = (SkatCard, SkatCard)

  def distribute(deck: List[SkatCard], players: List[Player]): Bidding = {
    import SkatCardSuites._

    require(players.size == SkatRules.PLAYER_COUNT)
    require(deck.size == allSuites.size * allCards.size)

    def distributeStacks(
                          deck: List[SkatCard],
                          acc: List[List[SkatCard]] = List(List(), List(), List(), List()),
                          takeCount: Int = 3): List[List[SkatCard]] =
      if (deck.isEmpty) acc
      else {
        val stack1 = acc(0) ++ deck.take(takeCount)
        val stack2 = acc(1) ++ deck.drop(takeCount).take(takeCount)
        val stack3 = acc(2) ++ deck.drop(takeCount).drop(takeCount).take(takeCount)

        if (acc(3).isEmpty) {
          val skat = deck.drop(takeCount).drop(takeCount).drop(takeCount).take(SkatRules.SKAT_CARDS)
          distributeStacks(
            deck.drop(SkatRules.PLAYER_COUNT * takeCount + SkatRules.SKAT_CARDS),
            List(stack1, stack2, stack3, skat),
            if (takeCount == 3) 4 else 3)
        } else {
          val skat = acc(3)
          distributeStacks(
            deck.drop(SkatRules.PLAYER_COUNT * takeCount),
            List(stack1, stack2, stack3, skat),
            if (takeCount == 3) 4 else 3)
        }
      }
    val stacks = distributeStacks(deck)

    Bidding(
      players(0).withCards(stacks(0)),
      players(1).withCards(stacks(1)),
      players(2).withCards(stacks(2)),
      (stacks(3).head, stacks(3).last)
    )
  }

  case class Player(id: String, cards: List[SkatCard] = List()) {
    def withCards(cards: List[SkatCard]) =
      Player(id, cards)

    def play(card: SkatCard): Player = withCards(cards - card)

    override def hashCode(): Int = id.hashCode

    override def equals(other: scala.Any): Boolean =
      other.isInstanceOf[Player] && other.asInstanceOf[Player].id == id
  }

  case class Bidding(player1: Player, player2: Player, player3: Player, skat: Skat) {
    var skatDrawn = false

    def takeSkat(declarer: Player) = {
      skatDrawn = true
      declarer.withCards(skat._1 +: skat._2 +: declarer.cards)
    }

    def completeBidding(declarer: Player, lastBid: Int, game: SkatGame, skat: Skat): Game = {
      val overBidding: Boolean = game match {
        case jg: JacksGame =>
          lastBid > jg.score / jg.jacksCounter * (jg.jacksCounter + JacksGame.SchneiderSchwartz.jacksOffset)
        case g: SkatGame =>
          lastBid > g.score
      }

      val validHand: Boolean = game match {
        case hg: HandGame => !skatDrawn
        case _ => true
      }

      require(!overBidding, "over bidding")
      require(validHand, "skat drawn")

      val allPlayers = List(player1, player2, player3)

      val defenders = allPlayers.filter(_.id != declarer.id)
      require(defenders.size == SkatRules.DEFENDER_COUNT)

      val declarerDroppedSkat = declarer.withCards(declarer.cards - skat._1 - skat._2)

      Game(declarerDroppedSkat, game, skat, allPlayers map {
        case Player(declarer.id, _) => declarerDroppedSkat
        case p: Player => p
      })
    }
  }

  trait SkatTrick {
    def playerCards: Map[Player, SkatCard]


    def opener: Player

    def openerCard: SkatCard

    def isTrump: Boolean

    def suite: FrenchCardSuite

    def matchSuite(card: SkatCard): Boolean


    def play(player: Player, card: SkatCard): Either[SkatTrick, Game]

    def owner: Player
  }

  object SkatTrick {
    def apply(opener: Player, openerCard: SkatCard)(implicit game: Game) = new GameTrick(opener, openerCard)(game)

    def apply(player1: Player, card1: SkatCard, player2: Player, card2: SkatCard, player3: Player, card3: SkatCard)(implicit game: Game): Game =
      SkatTrick(player1, card1)(game).play(player2, card2) match {
        case Left(withPlayer2) =>
          withPlayer2.play(player3, card3) match {
            case Right(g) => g
          }
      }

    protected case class GameTrick(playerCards: Map[Player, SkatCard])(implicit game: Game) extends SkatTrick {
      def this(opener: Player, openerCard: SkatCard)(implicit game: Game) =
        this(Map(opener -> openerCard))(game)

      require(playerCards.nonEmpty)
      require(playerCards.size <= SkatRules.PLAYER_COUNT)

      val opener = playerCards.head._1
      val openerCard = playerCards.head._2

      require(game.lastTurnPlayers.getOrElse(List(opener)).head == opener)

      val skatGame = game.game

      val isTrump = skatGame.isTrump(openerCard)
      val suite: FrenchCardSuite = openerCard.suite

      def matchSuite(card: SkatCard): Boolean =
        !skatGame.isTrump(card) && card.suite == suite

      def play(player: Player, card: SkatCard): Either[SkatTrick, Game] = {
        val followers = game.lastTurnPlayers.get -- playerCards.keys.toList

        require(followers.head == player)
        require(matchSuite(card) || player.cards.filter(matchSuite).isEmpty)

        val trick = GameTrick(playerCards ++ Map(player -> card))(game)

        if (followers.tail.isEmpty)
          Right(game.turn(trick))
        else
          Left(trick)
      }

      lazy val owner = playerCards.toList.sortBy(_._2)(skatGame.cardOrdering).head._1
    }

  }

  implicit def tricksToCards(tricks: List[SkatTrick]): List[SkatCard] =
    tricks.flatMap(t => t.playerCards.values)

  case class Game(declarer: Player, game: SkatGame, skat: (SkatCard, SkatCard), players: List[Player], //
                  allTricks: List[SkatTrick] = List(),
                  givingUpPlayers: List[Player] = List()) {
    require(players.size == SkatRules.PLAYER_COUNT)
    require(players.contains(declarer))

    def this(game: Game, lastTrick: SkatTrick) {
      this(game.declarer, game.game, game.skat, game.players, game.allTricks :+ lastTrick)
    }

    def this(game: Game, givingUpPlayers: List[Player]) {
      this(game.declarer, game.game, game.skat, game.players, game.allTricks, givingUpPlayers)
    }

    lazy val declarerTricks = allTricks filter {
      _.owner == declarer
    }

    lazy val defenders: List[Player] = players.filter(_ != declarer)

    def schenken(looser: Player): Game = schenken(List(looser))

    def schenken(loosers: List[Player]): Game = new Game(this, loosers)

    def gameOver(): Boolean = {
      lazy val gameSpecific = game match {
        case ng: NullGame => !ng.gameWon(declarerTricks)
        case _ => false
      }

      allTricks.size == SkatRules.TRICKS_TO_PLAY || gameSpecific || givingUpPlayers.nonEmpty
    }

    lazy val lastTurnOpt: Option[SkatTrick] = allTricks.lastOption

    lazy val lastTurnPlayers: Option[List[Player]] =
      if (lastTurnOpt.isEmpty)
        Some(players)
      else if (gameOver())
        None
      else Some {
        val last = lastTurnOpt.get
        val trickOwner = last.owner

        val i = players.indexOf(trickOwner)
        List(trickOwner,
          players.apply((i + 1) % players.size),
          players.apply((i + 2) % players.size))
      }

    def turn(trick: SkatTrick): Game = new Game(this, trick)

    def evaluate(): GameEvaluation = GameEvaluation(this)
  }

  case class GameEvaluation(game: Game) {
    require(game.gameOver())

    private def declaredGame = game.game

    val declarerCards: List[SkatCard] = game.skat._1 +: game.skat._2 +: tricksToCards(game.declarerTricks)
    require(declarerCards.size % 3 == 2, "expected n tricks of 3 cards and the skat with 2 cards")


    val hasDeclarerWon: Boolean = game.givingUpPlayers match {
      case List() => declaredGame.gameWon(declarerCards)
      case loosers: List[Player] => loosers match {
        case List(declarer) => false
        case List(player1, player2) =>
          def defenders = game.defenders
          if (defenders.contains(player1) && defenders.contains(player2)) true
          else false
      }
    }

    val declarerScore: Int = declaredGame.score match {
      case score: Int =>
        hasDeclarerWon match {
          case true => score
          case false => SkatRules.LOST_MULTIPLIER * score
        }
    }
  }

}
