{-# LANGUAGE ScopedTypeVariables #-}
module Search where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)

data Queue a = MkQueue [a] [a]

mkQueue :: Queue a
mkQueue = MkQueue [] []

queuePut :: a -> Queue a -> Queue a
queuePut item (MkQueue ins outs) = MkQueue (item:ins) outs

queuePutList :: [a] -> Queue a -> Queue a
queuePutList [] q = q
queuePutList (x:xs) q = queuePutList xs (queuePut x q)

queueGet :: Queue a -> (a, Queue a)
queueGet (MkQueue ins (item:rest)) = (item, MkQueue ins rest)
queueGet (MkQueue ins []) = queueGet (MkQueue [] (reverse ins))

queueEmpty :: Queue a -> Bool
queueEmpty (MkQueue ins outs) = (null ins) && (null outs)


bfs :: forall a. Int -> (a -> [a]) -> a -> (a -> Bool) -> Maybe [a]
bfs maxdepth succGenerator start goaltest =
    bfs' $ queuePut [start] mkQueue
    where
      bfs' :: Queue [a] -> Maybe [a]
      bfs' q | queueEmpty q = Nothing
             | goaltest item = Just path
             | length path > maxdepth = Nothing
             | otherwise = bfs' $ queuePutList (map (: path) $ succGenerator item) q'
             where (path, q') = queueGet q
                   item = head path

dfs :: forall a. Int -> (a -> [a]) -> a -> (a -> Bool) -> Maybe [a]
dfs maxdepth succGenerator start goaltest =
    dfs' maxdepth [start]
    where
      dfs' :: Int -> [a] -> Maybe [a]
      dfs' 0 _ = Nothing
      dfs' _ [] = Nothing
      dfs' d (item:stack)
          | goaltest item = Just [item]
          | otherwise = 
              case firstJust $ map (dfs' (d-1) . (: stack)) $ succGenerator item of
                Just p -> Just (item:p)
                Nothing -> Nothing
          where
            firstJust [] = Nothing
            firstJust ((Just x):xs) = Just x
            firstJust (_:xs) = firstJust xs


-- Iterative deepening DFS

iddfs :: forall a. Int -> (a -> [a]) -> a -> (a -> Bool) -> Maybe [a]
iddfs = iddfs' 1
    where
      iddfs' d maxdepth succ s0 goal
          | d > maxdepth = Nothing
          | otherwise = case dfs d succ s0 goal of
                          Just xs -> Just xs
                          Nothing -> iddfs' (d+1) maxdepth succ s0 goal



-- For heuritstic searches, the successor generator function must return
-- a list of tuples (c,state) where c represents the cost of making the
-- transition. The last parameter is the heuristic function h, mapping
-- states to the heuristic estimate. This replaces the goal-test function
-- as h(s) == 0 implies that s is a goal state.
-- The type parameter b represents the type of the cost/heuristic, usually
-- Int or a floating point type.

data OpenListNode a = 
    OpenListNode {
      state :: a,
      g_value :: Int,
      h_value :: Int
    }

-- type OpenList a b = (Map.Map

type OpenList a = (Map.Map Int [OpenListNode a], Map.Map a Int)

mkOpenList :: OpenList a
mkOpenList = (Map.empty, Map.empty)

emptyOL :: OpenList a -> Bool
emptyOL = Map.null . snd

addToOL :: forall a. (Ord a) => (a,Int,Int) -> OpenList a -> OpenList a
addToOL (state,g,h) (pq,members) =
    let node = OpenListNode { state = state, g_value = g, h_value = h }
    in
      if Map.member (g + h) pq then
          (Map.update (add node) (g + h) pq,
           Map.insert state (g + h) members)
    else
        (Map.insert (g + h) [node] pq, Map.insert state (g + h) members)
    where
      add :: OpenListNode a -> [OpenListNode a] -> Maybe [OpenListNode a]
      add node xs = 
          let (smaller,bigger) = span ((< h) . h_value) xs
          in
            return $ smaller ++ (node : bigger)
        


removeFromOL :: forall a. (Ord a) => a -> OpenList a -> OpenList a
removeFromOL stateToRemove (pq,members) =
    let bucket = fromJust $ Map.lookup stateToRemove members
        xs = fromJust $ Map.lookup bucket pq
    in
      (if length xs == 1 then
           Map.delete bucket pq
       else
           Map.update remove bucket pq,
       Map.delete stateToRemove members)
    where
      -- remove :: [OpenListNode a] -> Maybe [OpenListNode a]
      remove = return . (filter ((stateToRemove /=) . state))

-- This will return the cost of the node, if found
lookupOL :: (Ord a) => a -> OpenList a -> Maybe Int
lookupOL state (pq,members) = Map.lookup state members

getFirstOL :: OpenList a -> OpenListNode a
getFirstOL = head . snd . Map.findMin . fst

type ClosedList a = Set.Set a

mkClosedList :: (Ord a) => ClosedList a
mkClosedList = Set.empty

memberOfCL :: (Ord a) => a -> ClosedList a -> Bool
memberOfCL = Set.member

addToCL :: (Ord a) => a -> ClosedList a -> ClosedList a
addToCL = Set.insert


astar :: forall a. (Ord a) => (a -> [(Int,a)]) -> a -> (a -> Int) -> Maybe [a]
astar succ s0 h =
    astar' (addToOL (s0, 0, h s0) mkOpenList) mkClosedList
    where
      astar' :: OpenList a -> ClosedList a -> Maybe [a]
      astar' open closed
          | emptyOL open = Nothing
          | (h $ state $ getFirstOL open) == 0 = Just [state $ getFirstOL open]
          | otherwise =
              let node = getFirstOL open
                  newstates = filter (not . (`memberOfCL` closed) . snd) (succ $ state node)
                  open' = foldl (addToOpen (g_value node))
                                (removeFromOL (state node) open) 
                                newstates
                  closed' = addToCL (state node) closed
              in
                astar' open' closed'
      addToOpen :: Int -> OpenList a -> (Int,a) -> OpenList a
      addToOpen costSoFar open (cost,nextst) =
          case lookupOL nextst open of 
            Just previous_cost ->
                if costSoFar + cost < previous_cost then
                    addToOL (nextst, costSoFar + cost, h nextst) $ removeFromOL nextst open
                else
                    open
            Nothing -> addToOL (nextst, costSoFar + cost, h nextst) open

