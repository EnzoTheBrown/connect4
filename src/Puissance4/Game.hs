{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Puissance4.Game where
  import Data.Aeson
  import GHC.Generics


  data Pawn = Red | Yellow
    deriving (Show, Generic, Eq)
  instance ToJSON Pawn where
    toJSON pawn = object ["pawn" .= show pawn]

  oppositePlayer :: Pawn -> Pawn
  oppositePlayer x
    | x == Red = Yellow
    | otherwise = Red

  data Game = Game{
                  pawns::[(Pawn, Int, Int)],
                  turn:: Int,
                  player::Pawn
                  }
    deriving (Show, Generic, Eq)
  instance ToJSON Game where
    toJSON Game{..} = object
      [ "pawns" .= pawns
      , "turn" .= turn
      , "player" .= player
      ]
  initGame :: Game
  initGame = Game [] 0 Red

  addPawn :: Game -> Int -> Game
  addPawn game x = addPawn' game x 0

  addPawn' :: Game -> Int -> Int -> Game
  addPawn' game x y
    | x > 6 = game
    | (Red, x, 5) `elem` pawns game || (Yellow, x, 5) `elem` pawns game = game
    | (Red, x, y) `elem` pawns game || (Yellow, x, y) `elem` pawns game = addPawn' game x (y+1)
    | otherwise = (Game (((player game), x, y):(pawns game)) (turn game +1) (oppositePlayer $player game))

  countFromLast :: [(Pawn, Int, Int)] -> (Pawn, Int, Int) -> [Int]
  countFromLast game pawn =
    [(x!!0 + x!!1 - 1),(x!!2 + x!!3 - 1),(x!!4 + x!!5 - 1),(x!!6 + x!!7 - 1)]
    where x = [countFromLast' game pawn x 1 | x<-[0..7]]

  countFromLast' :: [(Pawn, Int, Int)] -> (Pawn, Int, Int) -> Int -> Int -> Int
  countFromLast' game (a, b, c) dir nb
    | dir == 0 && (a, b + 1, c) `elem` game = countFromLast' game (a, b + 1, c) dir (nb + 1)
    | dir == 1 && (a, b - 1, c) `elem` game = countFromLast' game (a, b - 1, c) dir (nb + 1)
    | dir == 2 && (a, b, c - 1) `elem` game = countFromLast' game (a, b, c-1) dir (nb + 1)
    | dir == 3 && (a, b, c + 1) `elem` game = countFromLast' game (a, b, c+1) dir (nb + 1)
    | dir == 4 && (a, b + 1, c - 1) `elem` game = countFromLast' game (a, b + 1, c-1) dir (nb + 1)
    | dir == 5 && (a, b - 1, c + 1) `elem` game = countFromLast' game (a, b - 1, c+1) dir (nb + 1)
    | dir == 6 && (a, b - 1, c - 1) `elem` game = countFromLast' game (a, b - 1, c - 1) dir (nb + 1)
    | dir == 7 && (a, b + 1, c + 1) `elem` game = countFromLast' game (a, b + 1, c+1) dir (nb + 1)
    | otherwise = nb

  gameComplete :: [(Pawn, Int, Int)] -> Bool
  gameComplete game =
    (maximum x) ==4
    where x = countFromLast game $head game

  drawSquare :: Int -> Int -> [(Pawn, Int, Int)] -> Char
  drawSquare x y game
    | (Red, x, y) `elem` game = 'X'
    | (Yellow, x, y) `elem` game = '0'
    | otherwise = ' '

  drawLine y game = [drawSquare x y game | x <- [0..6]] ++ "\n"
  drawGame game = concat [drawLine (-y + 5) game | y <- [0..5]]
  displayGame game = drawGame $pawns game


