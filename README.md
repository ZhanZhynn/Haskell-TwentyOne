# Haskell-TwentyOne
An AI player that competes in a game of TwentyOne developed in Haskell.

The goal is do implement an AI player which is able to:
- Parse Suit character (Spade, Club, Diamond, Heart) in BNF syntax.
- Parse Rank character (Ace, Two, Three, etc) in BNF syntax.
- Parse score by calculating the score on hand based on the cards given as input.
- Take actions during the "bidding" round, that is either to "Hit", "Double Down" or "Stand" by following a heuristic strategy, that is getting the highest possible hand value while not
risking going Bust.

More information on the code development and strategy implemented can be found in the file "FIT2102_report.pdf" located in the folder "submission".

### Disclaimer
As per assignment requirement, I only required to implement the Player.hs gameplay for the AI located in submission folder. The rest of the game logic has already been implemented in the code bundle.

