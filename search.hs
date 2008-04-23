{-# LANGUAGE ScopedTypeVariables #-}

module Search where

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


ida :: forall a. Int -> (a -> [a]) -> a -> (a -> Bool) -> Maybe [a]
ida = ida' 1
      where
        ida' d maxdepth succ s0 goal
            | d > maxdepth = Nothing
            | otherwise = case dfs d succ s0 goal of
                            Just xs -> Just xs
                            Nothing -> ida' (d+1) maxdepth succ s0 goal



-- For heuritstic searches, the successor generator function must return
-- a list of tuples (c,state) where c represents the cost of making the
-- transition. The last parameter is the heuristic function h, mapping
-- states to the heuristic estimate. This replaces the goal-test function
-- as h(s) == 0 implies that s is a goal state.
-- The type parameter b represents the type of the cost/heuristic, usually
-- Int or a floating point type.

-- data OpenListElement a b = OpenListElement {
      
-- type OpenList a b = (Map.Map

astar :: forall a b. Ord b => Int -> (a -> [(b,a)]) -> a -> (a -> b) -> Maybe [a]
astar maxdepth succ s0 h =
    Nothing
    where
      astar' :: Int -> OpenList -> ClosedList -> Maybe [a]
      astar' d open closed | d > maxdepth = Nothing
                           | emptyOL open = Nohting
                           | h node == 0  = Just build_solution
                           | otherwise =
                               let newnodes = filter (notOnCL closed) (succ node)
                                   open' = foldl addToOpen open newnodes
                                   closed' = addToCL closed node
                               in
                                 astar' (d+1) open' closed'
                               where
                                 addToOpen :: OpenList -> Node -> OpenList
                                 addToOpen open node =
                                     if onOL open node then
                                         if node is better then
                                             addNode (removeNode open oldnode) node
                                         else
                                             open
                                     else
                                         addNode open node

