module Lib where

import GraphSearch
import GraphSearch.AStar

type Card = Int

data BoardState
  = BoardState
  { deck :: [Card]
  , piles :: [[Card]]
  } deriving (Eq, Ord, Show)

data End = Bottom | Top deriving (Eq, Ord, Show)

data Operation = PlaceCard End Int | Stack Int Int deriving Show

swap :: (a, a) -> (a, a)
swap (a1, a2) = (a2, a1)

choose2 :: Int -> [(Int, Int)]
choose2 = \case
    n | n < 2 -> []
    (sub1 -> pred) -> choose2 pred <> ((, pred) <$> take pred [0..])
  where
    sub1 x = x - 1

isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [a] = True
isSorted (a1 : a2 : as) = a1 < a2 && isSorted (a2 : as)

instance SearchProblem BoardState Operation Int where
  legalMoves (BoardState d ps) = case d of
      [] -> uncurry Stack <$> stacks
      _
        | not (null stacks) -> uncurry Stack <$> stacks
        | otherwise -> uncurry PlaceCard <$> places
    where
      allstacks = mappend c2 $ swap <$> c2
      stacks = filter stackable allstacks
      stackable (a, b) = last (ps !! a) == head (ps !! b) - 1
      allplaces = [(Top,), (Bottom,)] <*> take (lps + 1) [0..]
      places = filter placeable allplaces
      placeable (Top, i) = if lps > i then head d == head (ps !! i) - 1 else True
      placeable (Bottom, i) = if lps > i then last d == head (ps !! i) - 1 else True
      c2 = choose2 lps
      lps = length ps

  cost = const 1

  terminal (BoardState d ps) = length d == 0 && length ps == 1 && isSorted (ps !! 0)

  move = curry $ \case -- super gross / crash-prone on bad input; eh, who cares, if it works
    (PlaceCard e i, BoardState d ps) -> case e of
        Top -> BoardState (tail d) (place (head d) ps i)
        Bottom -> BoardState (init d) (place (last d) ps i)
      where
        place card = curry $ \case
          (p : piles, 0) -> (card : p) : piles
          ([], _) -> [[card]]
          (p : piles, n) -> p : place card piles (n - 1)
    (Stack a b, BoardState d ps) -> BoardState d $ leaveOut a $ stack (ps !! a) b ps -- ugh, fine, maybe I should learn to use lenses
      where
        stack pile 0 (p : piles) = (pile <> p) : piles
        stack pile i (p : piles) = p : stack pile (i - 1) piles
        stack pile i [] = error "walked off the end of the list while trying to stack"
        leaveOut i l = take i l <> drop (i + 1) l


cardheuristic :: BoardState -> Int
cardheuristic (BoardState d ps) = length d + length ps - 1


optimalCardSort :: [Card] -> Maybe [Operation]
optimalCardSort d = reverse <$> astar cardheuristic [BoardState d []]


intermediateStates :: (SearchProblem s o c) => s -> [o] -> [s]
intermediateStates s  = \case
  [] -> []
  (flip move s -> s') : os -> s' : intermediateStates s' os
