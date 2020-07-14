{-
Author: Sagar A. Chitnis
Purpose: Play Find the Ships with two players: Hider and Searcher

In this two player game the Hider decides the hide the location of
3 ships on a 4-row 8-column grid. The ships are of unit size, at different
locations and their positions are hidden from the second player: Searcher.
The Searcher makes a guess comprising of three different location. The hider
provide a feedback consisting of three numbers indicating the number of 
correct guesses, number of guesses that are one space away and number of 
guesses that are two spaces away. Based on this information the Searcher 
makes the next guess and gets the next feedback. The game continues till
the Searcher gets all the hidden locations correctly and feedback shows (3,0,0).
-}

module Proj1 (Location, toLocation, feedback,
              GameState, initialGuess, nextGuess) where
                                                                                
import Data.Char
import Data.List

--Below data design signifies a position on the grid.
data Location = Loc Char Int     --Char = Column, Int = Row
       deriving (Eq)

--Below type stores the list of list of type: Location, corresponding 
--to the remaining guess combinations the Searcher can decide
data GameState = Gs [[Location]]
       deriving (Show)

instance Show Location where     -- Simple String represtation of type Location 
   show (Loc column_aplhabet row_number) = [column_aplhabet] ++ show row_number
    

toLocation :: String -> (Maybe Location)
--Returns the location if it is within grid, else nothing
toLocation ip_location
            | length ip_location /= 2 = error "Check Length of Location"
            | isInputLocationWithinGrid column row = (Just (Loc column row))
            | otherwise = Nothing
           where column = head ip_location
                 row = (read (tail ip_location) :: Int)

isInputLocationWithinGrid :: Char -> Int -> Bool
--Validates if contents of the input are within the grid row & columns limits
isInputLocationWithinGrid alpha_column num_row = 
   (alpha_column >= 'A') && (alpha_column <= 'H') --Columns between 'A' & 'H'
       && (num_row >= 1 && num_row <= 4) 


feedback :: [Location] -> [Location] -> (Int,Int,Int)
--Returns the feedback based on the targets and guesses
--Throws an error if one passes incorrent inputs for guesses and targets
feedback targets guesses
            | length targets < 3 = error "Insufficient Targets"
            | length targets > 3 = error "Exceeded number of Targets"
            | areLocationsNotUniqueIn targets = error "Targets are not unique"
            | areLocationsNotUniqueIn guesses = error "Guesses are not unique"
            | otherwise = calculateFeedback targets guesses hits one two
        where hits = 0   --counter for correct guesses in target
              one = 0    --counter for one distance away targets from guess
              two = 0    --counter for two distance away targets from guess
        
areLocationsNotUniqueIn :: [Location] -> Bool
--Checks if every input of targets/guesses have unique locations
areLocationsNotUniqueIn (loc1:loc2:loc3:ts)
     | ((loc1 == loc2) || (loc1 == loc3) || (loc2 == loc3)) = True 
     | otherwise = False

calculateFeedback :: [Location]-> [Location]-> Int-> Int-> Int-> (Int,Int,Int)
--Keep counter for hits and single,double distances for guesses from targets
--Returns the final counter values to feedback function
calculateFeedback target [] hit one two = (hit, one, two)
calculateFeedback target (guess:gs) hit one two
     | guess `elem` target = calculateFeedback target --target present in guess 
                                             gs (hit+1) one two 
     | getMinDistance guess target == 1 = calculateFeedback
                                             target gs hit (one+1) two
     | getMinDistance guess target == 2 = calculateFeedback 
                                             target gs hit one (two+1)
     | otherwise = calculateFeedback target gs hit one two

getMinDistance :: Location -> [Location] -> Int
--Helper function returns the minimum distance of one guess for all the targets
getMinDistance guess targets =  minimum (distance guess targets)

distance :: Location -> [Location] -> [Int]
--Calculates and returns the distances for one guess over all targets in grid
distance guess [] = []
distance guess (target:ts) =  [euclideanDistance guess target] ++ 
                                                        distance guess (ts)

euclideanDistance :: Location -> Location -> Int
--Returns the distance of each guess from each target
euclideanDistance (Loc guess_column guess_row) (Loc target_column target_row)
     | dis >= 2 && (dis < 4) = 1
     | dis >= 4 && (dis <= 8) = 2
     | otherwise = dis
    where dis = ((ord guess_column - (ord target_column))^2 
                                   +  (guess_row - target_row)^2)
                                   
convertLocationString :: Location -> String
--Converts the Location Design into a simplified String presentation
convertLocationString location = show location


{-
The initial guess is hardcoded after experimenting and trying different
heuristics that could give the best results. Spacing the 3 guess locations 
equidistantly in the grid gives bad results. Thus we go ahead with the idea of
keeping guesses together (A1,A2,A3) and hoping for a feedback (0,0,0)
as that will eliminate almost 1/3rd of the grid.
-}
initialGuess :: ([Location],GameState)
initialGuess = (generateLocationGrid ['A'] [1..3],  -- Locations A1, A2, A3
                 (Gs (gridcombinations 3 
                           (generateLocationGrid ['A'..'H'] [1..4]))))

generateLocationGrid :: [Char] -> [Int] -> [Location]
--Generate and returns continuous grid locations from A1 to H4
generateLocationGrid alphaList rowList =
                              [(Loc x y) | x <- alphaList, y <- rowList]

{-
The next guess function takes the previous guess, feedback and all
the combinations of the grid in the gamestate. Thus the a grid combinations act
as targets and we try calculating the feedback for the every combination with
the previous guess. We retain only those combinations where we got a feedback
similar to the previous guess and real target. We then choose the middle 
element of the list as the next guess.
-}
nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (guess, (Gs (comb:combs))) (hit,single,double) =
       do
         let same_comb = getSameFeedBackCombs (guess, (comb:combs)) 
                                                         (hit,single,double) []
         let mid_value = (length same_comb) `div` 2
         let next_guess = same_comb !! mid_value
         (next_guess, (Gs same_comb))

getSameFeedBackCombs :: ([Location],GameState) -> (Int,Int,Int) -> [[Location]] -> [[Location]]
--Returns Grid Combinations with Feedback equal to previous guess and target
getSameFeedBackCombs (guess,[]) (hit,one,two) result = result
getSameFeedBackCombs (guess, ((combination:cs))) 
                                     (hit,one,two) result
     | feedback combination guess == (hit,one,two) =
             result ++ [combination] ++ (getSameFeedBackCombs 
                                     (guess, (cs)) (hit,one,two) result)
     | otherwise = getSameFeedBackCombs (guess, (cs)) 
                                     (hit,one,two) result


gridcombinations :: Int -> [Location] -> [[Location]]
--Returns all the combinations based in a group of the integer passed as input
gridcombinations 0 (loc:ls) = [[]]
gridcombinations _ [] = []
gridcombinations n (loc:ls) = ((map (loc:) (gridcombinations (n-1) ls)) ++ 
                                                       (gridcombinations n ls))
