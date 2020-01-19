module Lib where

import GraphSearch

import Control.Monad.State
import Data.Map hiding (empty, elem, map, insert, filter, drop, take, null)
import Data.Set hiding (fromList, minView, map, size, union, filter, drop, take, null)

import Debug.Trace


type Card = Int

data BoardState
  = BoardState
  { deck :: [Card]
  , piles :: [[Card]]
  } deriving (Eq, Ord, Show)

data End = Bottom | Top deriving (Eq, Ord, Show)

data Operation = PlaceCard End Int | Stack Int Int deriving Show

{-# INLINE swap #-}
swap :: (a, a) -> (a, a)
swap (a1, a2) = (a2, a1)

{-# INLINE choose2 #-}
choose2 :: Int -> [(Int, Int)]
choose2 = \case
    n | n < 2 -> []
    (sub1 -> pred) -> choose2 pred <> ((, pred) <$> take pred [0..])
  where
    sub1 x = x - 1

{-# INLINE isSorted #-}
{-# SPECIALIZE isSorted :: [Int] -> Bool #-}
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

  cost _ = \case
    Stack _ _ -> 1
    _ -> 0

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
cardheuristic (BoardState d ps) = length ps - 1 -- length d + length ps - 1


optimalCardSort :: [Card] -> Maybe [Operation]
optimalCardSort d = reverse <$> astar cardheuristic [BoardState d []]


intermediateStates :: (SearchProblem s o c) => s -> [o] -> [s]
intermediateStates s  = \case
  [] -> []
  (flip move s -> s') : os -> s' : intermediateStates s' os

-- ASTAR
type Heuristic s c = s -> c

data AStarState s o c = AStarState { visited :: Set s, frontier :: Map (c, s) (Path o c) }

data Path o c = Path { pathOps :: [o], pathCost :: c }

{-# INLINE astarStep #-}
{-# SPECIALIZE astarStep :: Heuristic (BoardState) Int -> State (AStarState BoardState Operation Int) (Maybe (Maybe [Operation])) #-}
astarStep
  :: (Ord s, SearchProblem s o c, MonadState (AStarState s o c) m)
  => Heuristic s c -> m (Maybe (Maybe [o]))
astarStep h = do
  AStarState vis fron <- get
  case minViewWithKey fron of
    Nothing -> pure $ Just Nothing -- no paths found to terminal nodes
    Just (((c, s), p@(Path ops pc)), fron')
      | elem s vis -> state $ const (Nothing, AStarState vis fron')
      | terminal s -> pure $ Just $ Just $ reverse ops
      | otherwise -> state $ const (Nothing, AStarState (insert s vis) $ trace (show $ size fron'') fron'')
        where
          costToCome = pc
          nextOps = legalMoves s
          nextCosts = cost s <$> nextOps
          s's = map (flip move s) nextOps
          hs = map (uncurry (+)) $ zip nextCosts $ map (+ costToCome) $ map h s's
          newFron = fromList $ filter (not . flip elem vis . snd . fst) $ zip (zip hs s's) $ uncurry Path <$> zip (map (: ops) nextOps) (map (+ pc) nextCosts)
          fron'' = union fron' newFron

{-# INLINE runUntilJust #-}
runUntilJust :: State s (Maybe a) -> s -> a
runUntilJust m = go where
  go s = case runState m s of
    (Just a, _) -> a
    (_, s') -> go s'


astar :: (Ord s, SearchProblem s o c) => Heuristic s c -> [s] -> Maybe [o]
astar h ss = runUntilJust (astarStep h) $ AStarState empty frontierInit
  where
    frontierInit = fromList $ zip (zip (h <$> ss) ss) $ repeat (Path [] 0)

