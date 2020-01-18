module GraphSearch where

class (Num c, Ord c) => SearchProblem  s o c | s -> o, s -> c, o -> s where
  legalMoves :: s -> [o]
  move :: o -> s -> s
  cost :: s -> o -> c
  terminal :: s -> Bool

