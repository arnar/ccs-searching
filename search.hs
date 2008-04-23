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
