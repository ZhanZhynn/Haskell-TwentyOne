-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game

-- You can add more imports if you need them
import           Parser.Instances      
import           Data.List      
import           Data.Char

import Debug.Trace

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard upCard score pinf pid Nothing hand = playCard upCard score pinf pid (Just(";" ++ show pScore)) hand
    where
        pScore = find' getPoints pid getId score --get player's current score

playCard upCard score _ pid (Just memory) hand = action newMemory
    where
        newMemory 
            | upCard == Nothing = ";" ++ show pScore
            | otherwise = memory
                where 
                    pScore = find' getPoints pid getId score -- get player's current score

        action m            
            -- during bidding round, reinitiate memory to store player's latest score
            | trace ("bid: pid=" ++ show pid ++ " hand: " ++ show hand ++ " memory: " ++ m ++ " score: " ++ show score ++ 
                " 2nd lowest score: " ++  show (getPoints secondLowest) ++ 
                " memoryCount: " ++ show (length (getResult (parse parseHand (m)))) ) False = undefined
            | upCard == Nothing = (Bid minBid, ";" ++ show pScore) 

            -- if low card value, Hit
            | handCalc hand <= 9 = (Hit, show c ++ m)

            -- Double Down Action Block
            -- Double Down when hand value is 11 or 10
            -- Down Down only possible after the bidding round and when you have exactly two cards on hand
            -- Precaution is taken to make sure pScore (player points) is sufficent for Double Down
            | (handCalc hand == 11 || handCalc hand == 10) && 
                length (getResult (parse parseHand (m))) == 0 && 
                reverse (getResult (parse parseScore (reverse m))) == show (pScore + minBid) &&
                pScore > (minBid*2)
                    = (DoubleDown (minBid), m) 
            -- After doubleDown, must Hit and then Stand
            -- If difference in current and memery score is twice initial bid, means the previous actions must be doubleDown.
            | (handCalc hand == 11 || handCalc hand == 10)  && reverse (getResult (parse parseScore (reverse m))) == show (pScore + minBid*2) && length (getResult (parse parseHand (m))) == 0 
                = (Hit, show c ++ m) 
            | length (getResult (parse parseHand (m))) == 1 && reverse (getResult (parse parseScore (reverse m))) == show (pScore + minBid*2)
                = (Stand, m) 

            -- Try Charlie if hand has 4 cards (3 cards in memory), largest card in memory in small and hand value < 17
            | handCalc hand < 17 && length (getResult (parse parseHand m)) == 3 && handCalc ( getResult (parse parseHand m) ) < 8 = (Hit, show c ++ m) -- Hit and remember largest card

            -- Strategy based on player hand value and dealer's up card
            -- Try to get highest hand value without going Bust
            | handCalc hand == 12 && sum (toPoints <$> upCard) >= 2 && sum (toPoints <$> upCard) <= 6 = (Stand, m)
            | handCalc hand == 12 && sum (toPoints <$> upCard) >= 7 && sum (toPoints <$> upCard) <= 10 = (Hit, show c ++ m) 
            | handCalc hand >= 13 && handCalc hand < 17 && sum (toPoints <$> upCard) >= 2 && sum (toPoints <$> upCard) <= 6 = (Stand, m)
            | handCalc hand >= 13 && handCalc hand < 17 && sum (toPoints <$> upCard) >= 7 && sum (toPoints <$> upCard) <= 10 = (Hit, show c ++ m) 
            | handCalc hand >= 17 = (Stand, m)
            | otherwise = (Hit, show c ++ m) 
                
                where 
                    pScore = find' getPoints pid getId score --get player's score
                    d = reverse $ sortOn getRank hand -- sort the cards in descending order
                    c = head d  --largest card on hand 

                    -- coded but not actually used as a simpler strategy is used
                    scoreBoard = sortOn getPoints score -- score board in ascending order
                    secondLowest = head $ tail scoreBoard  -- 2nd lowest score in score board
                

-- | Parsers used to parse string stored in player's memory

-- | This parser parses Suit character
parseSuit :: Parser Suit
parseSuit = (is 'S' >> pure Spade) ||| 
            (is 'C' >> pure Club) ||| 
            (is 'D' >> pure Diamond) ||| 
            (is 'H' >> pure Heart)

-- | This parser parses Rank character
parseRank :: Parser Rank
parseRank = 
    (is 'A' >> pure Ace) ||| 
    (is '2' >> pure Two) |||
    (is '3' >> pure Three) ||| 
    (is '4' >> pure Four) |||
    (is '5' >> pure Five) ||| 
    (is '6' >> pure Six) |||
    (is '7' >> pure Seven) ||| 
    (is '8' >> pure Eight) |||
    (is '9' >> pure Nine) ||| 
    (is 'T' >> pure Ten) |||
    (is 'J' >> pure Jack) ||| 
    (is 'Q' >> pure Queen) |||
    (is 'K' >> pure King) 
    
-- | This parser parses a Card string of Suit and Rank characters
parseCard :: Parser Card
parseCard = parseSuit >>= (\s -> parseRank >>= (\r -> pure (Card s r)))

-- | This parser parses concatenated Card string
parseHand :: Parser Hand
parseHand = list parseCard

-- | This function get the useful data types from the ParseResult
getResult :: ParseResult t -> t
getResult (Result _ c) = c
getResult _ = error "Wrong Parsing"

-- | This parser parses all digit character in a string
parseScore :: Parser String
parseScore = list digit

-- | This parser returns a parser that continues producing a list of values from the given parser
list :: Parser a -> Parser [a]
list k = list1 k ||| pure []

list1 :: Parser a -> Parser [a]
list1 p = do
  p' <- p
  p'' <- list p
  pure (p':p'')

-- | This parser parses if the digit character satisfies the condition
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = character >>= 
    (\c -> if (not (f c)) then (unexpectedCharParser c) else pure c)

-- | This parser parses a digit character
digit :: Parser Char
digit = satisfy isDigit
