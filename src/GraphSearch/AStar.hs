module GraphSearch.AStar where

import Control.Monad.State hiding (fail)
import Data.Map hiding (empty, elem, map, insert)
import Data.Set hiding (fromList, minView, map)

import GraphSearch

type Heuristic s c = s -> c

data AStarState s o c = AStarState { visited :: Set s, frontier :: Map (c, s) (Path o c) }

data Path o c = Path { pathOps :: [o], pathCost :: c }

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
      | otherwise -> state $ const (Nothing, AStarState (insert s vis) fron'')
        where
          costToCome = pc
          nextOps = legalMoves s
          nextCosts = cost s <$> nextOps
          s's = map (flip move s) nextOps
          hs = map (uncurry (+)) $ zip nextCosts $ map (+ costToCome) $ map h s's
          fron'' = fromList $ zip (zip hs s's) $ uncurry Path <$> zip (map (: ops) nextOps) (map (+ pc) nextCosts)

runUntilJust :: State s (Maybe a) -> s -> a
runUntilJust m = go where
  go s = case runState m s of
    (Just a, _) -> a
    (_, s') -> go s'


astar :: (Ord s, SearchProblem s o c) => Heuristic s c -> [s] -> Maybe [o]
astar h ss = runUntilJust (astarStep h) $ AStarState empty frontierInit
  where
    frontierInit = fromList $ zip (zip (h <$> ss) ss) $ repeat (Path [] 0)

