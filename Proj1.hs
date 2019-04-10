{-
  Student ID: 878637
  Student Name: Junhan Liu
  
  The ChordProbe guessing strategy programe. This programe works like a 
  pitch guessing strategy that firstly taking three hard coded pitches as initial
  guess. After getting the distance between the real chord and guess to compute the 
  most ideal lists of possible chord and narrow down the possible answer to locate
  the right chord.
  
  Strategy explanation: This strategy use hard coded chord as the first initial
  input to get the first distance,how many right pitch, note and octova, between the 
  initial input and right answer. And Gamestate is every possible combination of the 
  chord from A to G and 1 to 3 (there are 1330 difference, excluding same pitch and 
  different order) as the first State.

  After getting the distance from initial chord and right wnswer, the programe first 
  assumes the initial input is the right answer, (3,0,0), and calculate the distance for
  every possible chords in the Gamestate, and pick up the chords that distance is the same 
  as the distance that retured from the game.

  After getting the narrowed Gamestate, the the next guess chord is the first element from 
  the Gamestate list, and return it.

  The programe will finally narrow down to the only one element, that is the right wnswer.
-}

module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List
  
-- the GameState type that contains list of list of string, that stores the candidate of
-- the possible answers
type GameState = [[String]]

--Create combination of Pitches from A to G and 1 to 3
pitch :: [String]
pitch = [ charToString(x,y) | x <- ['A'..'G'] ,y<- ['1'..'3']]


--Convert char to string so that it can be converted to string for later manipulate of data
charToString :: (Char,Char) -> String
charToString (x,y) = [x]++[y]


--Create the every possibility of the chord by constructing the three pitches,
--repeated chord and Same pitch are excluded
possibleGuess ::[[String]]
possibleGuess = [[x1,x2,x3] | x1 <-pitch,x2 <-pitch,x3 <-pitch, x1<x2 && x2<x3]


--Take the initial hard coded guess and initial every possibility of the chords as the 
--output
initialGuess ::([String],GameState)
initialGuess = (["A3","C2","D2"], possibleGuess)


-- The first input comes from the previous guess and GameState, the second argument 
-- is the distance between the last guess and right answer
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess value b = (guess,state)
 where 
  guess = last state
  state = calcDistance value b


--The first argument comes from the previous guess and the game state, this function
--firstly assume the previous guess as the right chord and calculate every distance 
--in the GameState based on previous guess chord. Then the second argument acts as the 
--filter to pick up every chord in the GameState that has the same distance with the 
-- second argument as the candidate, and return getCandidate as the output.
calcDistance :: ([String],GameState) -> (Int,Int,Int) -> GameState
calcDistance(a,[]) b = []
calcDistance preGuess distance = getCandidate
  where 
    getCandidate = calcCurrentDistance preDistance distance
    preDistance = createDistance preGuess
    

--This function is used for calculating the dictance based on previous guess
--the input is a tuple,the first element is previous guess, and the second element
--is the previous game state.
--This function assumes the previous guess chord is the right answer, and calculate
--every didtance of chord in the GameState and returns the list of tuples, the first
--element in the tuple is the chord distance between previous guess, and second element
--is the chord in the gamestate
createDistance :: ([String],GameState) -> [((Int,Int,Int),[String])]
createDistance (string,[])= []
createDistance (chord,x:xs) 
  | null x /= True = [(calValue chord x, x)] ++ createDistance (chord,xs)
  | otherwise = []


--This function takes two argument, the first one is the distance based on 
--the previous guess,which is a list of tuples, the second argument is the 
--system feedback for the distance to the right answer based on the previous guess
--the function is going to pick up the chords in the first argument that have the 
--same distance with the feedback, and output a GameState for next guess
calcCurrentDistance :: [((Int,Int,Int),[String])] -> (Int,Int,Int) -> GameState
calcCurrentDistance [] b = []
calcCurrentDistance (x:xs) b 
  | compareDist x b  = [snd x] ++ calcCurrentDistance xs b
  | otherwise = calcCurrentDistance xs b


--This function takes two arguments, one is a tuple, another is the 
--feedback that indicates the distance tio the right answer based on 
--the previous chord guess, if the distance of the chord is the same as
--the feedback, it will retuen true, else it returns false.
compareDist :: ((Int,Int,Int),[String]) ->(Int,Int,Int) -> Bool
compareDist a b 
   | fst a `elem` [b] = True
   | otherwise  = False



--This function is to calculate the distance between two chords,
--the input is two chords, the output is the distance between two chords
--the pitch distance is calculated by the intersection between two chords,
--which means the same pitch between two chords.
--The distance of the note is calculated by the number of elements of chord
--minus the remaining elements in the first argument, teh deleteFirst by 
--is to delete the note of octova if it first appears in the second chord
--because the length(deleteFirstsBy (noteOrOctova "note") posTarget guess) function
--also calculated the right pitch, so we should delete the right pitch to calculate
--the right note between two chords, so as disOct.
calValue ::[String] -> [String] -> (Int,Int,Int)
calValue posTarget guess =(disPitch,disNote,disOct)
  where
    disPitch = length(intersect guess posTarget)
    size = length guess
    disNote = size - 
              length(deleteFirstsBy (noteOrOctova "note") posTarget guess) - disPitch
    disOct = size - 
              length(deleteFirstsBy (noteOrOctova "octova") posTarget guess) - disPitch


--The note is the first character of the string, so the index is o
--The octova is the second element of the string, so the index of the 
--octova is 1, and returns true when two notes or octova are the same
noteOrOctova ::String -> String -> String -> Bool
noteOrOctova a l1 l2
  | a == "note" = head l1 == head l2
  | a == "octova" = tail l1 == tail l2
  | otherwise = False
  


