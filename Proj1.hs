-- Keming Zhang
-- Student No. 813368
-- COMP30020 Declarative Programming

--Implement your solution here
--Remember to put function declarations as well 

------------------------------------------------------------------------------

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

    import Card
    import Data.List
    import Data.Maybe
    import Data.Function
    
    type Guess = [Card]       -- Guess is list of cards
    type GameState = [Guess]     -- GameState should store guesses
    type Score = (Int, Int, Int, Int, Int)
    
    ------------------------------------------------------------------------------
    -- Return a score based on two input, which describes the accuracy
    -- of the guess. A total correct means that both rank and suit are 
    -- the same in two guesses. 
    
    feedback :: Guess -> Guess -> Score
    feedback anwser guess = 
        (correctCards, lowerRank, correctRank, higherRank, correctSuit)
            where 
                totalNumber = length $ nub guess
                correctCards = length $ intersect guess anwser 
                -- The deleteFirstsBy function takes a predicate and two lists and 
                -- returns the first list with 
                -- the first occurrence of each element of the second list removed.
                correctRank = length $ nub [card | card <- guess, anw <- anwser, rank card == rank anw]
                --totalNumber - correctCards -
                --length (deleteFirstsBy sameRank anwser guess)
                correctSuit = length $ nub [card | card <- guess, anw <- anwser, suit card == suit anw]
                --totalNumber - correctCards - 
                --length (deleteFirstsBy sameSuit anwser guess)
                lowerRank = length [card | card <- guess, card < (sortBy compare anwser) !! 0]
                higherRank = length [card | card <- guess, card > (sortBy compare anwser) !! (length anwser - 1)]
    ------------------------------------------------------------------------------
    -- Make an initial guess. Generate all possible guesses and store in a state
    -- Return the first guess and the game state
    
    initialGuess :: Int -> (Guess, GameState)
    initialGuess deckSize = (initialGuess, state)
            where
                cards = [Card (read suit) (read rank) | suit <- ["C","D","H","S"], 
                                                        rank <- ["2","3","4","5","6","7","8","9","T","J","Q","K","A"]]
                allStates = [[a] | a <- cards]     
                initialGuess = take deckSize cards
                state = allStates \\ [initialGuess]
    ------------------------------------------------------------------------------
    -- Make a next guess from the updated state, given the latest guess and score. 
    
    nextGuess :: (Guess, GameState) -> Score -> (Guess, GameState)
    nextGuess (lastGuess, state) score = (newGuess, newState)
      where 
        -- Cut the state such that only the guesses that 
        -- have the same score will be remained. 
        newState = delete lastGuess [candidate | candidate <- state,
          feedback candidate lastGuess == score] 
        newGuess  = selectGuess newState
    
    ------------------------------------------------------------------------------
    -- Compute the average number of possible cards that will remain 
    -- after each guess. For each possible guesses in the remaining state, 
    -- calculate the utility score for the guess. Then based on the 
    -- effeciency performance of each guess, select the most effective 
    -- one (with the most minimum possible guesses remaining.)
    
    selectGuess :: GameState -> Guess 
    selectGuess state = bestGuess -- The first one should be the most effective. 
      where 
        candidates = [(guess, utilityScore)
          | guess <- state, 
          let utilityScore = utility guess state]
        guesses = sortBy (compare `on` snd) candidates 
        bestGuess = fst (head guesses)
        -- Sort all remaining choices based on the effeciency.
                            
    ------------------------------------------------------------------------------
    -- Calculate the expected number of possible guesses that will be left after  
    -- a guess were made. Group the result that have the same score and divided
    -- by the total possibilities times the group number. This is the efficiency 
    -- score for one guess. Calculate the utility score for each guess and sort
    -- them. The fewer score it has, the more efficient the guess is. 
    -- Note that here 'fromIntegral' is necessary since we need division to 
    -- calculate the percentage. 
    
    utility :: Guess -> GameState -> Double
    utility lastGuess state = sum [(numPos / total) * numPos| g <- groupedScores, 
      let numPos = (fromIntegral.length) g]
      where 
        totalScores = [score | guess <- state, 
          let score = feedback lastGuess guess]
        total = (fromIntegral.length) state
        groupedScores = (group.sort) totalScores         
    
    ------------------------------------------------------------------------------
    -- Given two lists and the postion we are interested in, return true
    -- If and only if the nth element in two lists are matching the requirment. 
      
    --sameSuit :: Card -> Card -> Bool
    --areTheSame anwser guess = suitA == suitG
    --            where suitA = suit anwser
    --                  suitG = suit guess
    
    --sameRank :: Card -> Card -> Bool
    --sameRank anwser guess = rankA == rankG
    --            where rankA = rank anwser
    --                  rankG = rank guess
    
    --compRank :: (Card -> Card -> Bool) -> Card -> Card -> Bool
    --compRank op anwser guess = (suit anwser) `op` (suit guess)