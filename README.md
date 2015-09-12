# examples.cards

Educational scala project based on the topic of card games, e.g. [Skat](https://en.wikipedia.org/wiki/Skat_%28card_game%29).

## Challenges

Intended to be used for succeeding trails:

### Trail: Understand the framework

1. Find the major game phases. Which are the classes representing the game phases?
2. Find the methods that end each game phase and create a representation of the next.

### Trail: Bidding

2. Write some code
    1. Implement [bidding mechanics](https://en.wikipedia.org/wiki/Skat_(card_game)#Bidding_mechanics).
        * Write an extra layer on top of SkatGameFactory which should:
        * Rotate players regarding bidding and the first game round.
    2. Test that feature.
        * Simple case could be all second seat player calls and third and first seat player pass.
        * Second simple case second seat player passes, so that first seat player resumes as caller.
        * Complex case should include several accepted calls.

3. Extend AI capabilities of ComputerPlayer/RandomPlayer
    1. Make RandomPlayers recognize their current role in bidding and react accordingly.
        * Allow RandomPlayers to bid up the limits of over-bidding.
        * RandomPlayers can pass occasionally.
    2. Test the RandomPlayer capabilities.
        * Based on different hand cards RandomPlayer should indicate its limit and take a random but correct decision.

### Trail: Advanced AI

4. Create a ComputerPlayer that plays wins more games than RandomPlayer against RandomPlayer.
    1. As declarer playing solo.
    2. As defender eventually collaborating when tricks is owned by same party.
    3. Hybrid of two above.

5. Evolve ComputerPlayer to have balanced win/loose-ratio convergence playing against other ComputerPlayers of that kind.