module Jarvis where
  import Game

  cost game =
    countFromLast (pawns game) (head $pawns game)
