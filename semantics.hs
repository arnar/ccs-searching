module Semantics where

import CCS

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub)
import Data.Maybe (catMaybes)

ccsSucc :: ProcessDefinitions -> Process -> [(Action, Process)]

-- Rule ACT
ccsSucc defs (Prefix a p) = [(a, p)]

-- Rule SUM
ccsSucc defs (Choice ps) = nub $ concatMap (ccsSucc defs) ps

-- Rules COM1 - COM3
ccsSucc defs (Parallel p q) = nub $ concat [
                               map (wrapState (\x -> Parallel x q)) lefts,
                               map (wrapState (\x -> Parallel p x)) rights,
                               catMaybes $ map (uncurry sync) [(l,r) | l <- lefts, r <- rights]
                              ]
                              where
                                lefts  = ccsSucc defs p
                                rights = ccsSucc defs q
                                wrapState f (a, p) = (a, f p)
                                sync (a,p) (a',p')
                                     | a == complement(a') = Just (Tau, Parallel p p')
                                     | otherwise = Nothing

-- Rule RES
ccsSucc defs (Restrict p r) = nub $ map wrap $ filter f $ ccsSucc defs p
                              where
                                f (a,_) = (Set.notMember a r) && (Set.notMember (complement a) r)
                                wrap (a,p') = (a, Restrict p' r)

-- Rule REL
ccsSucc defs (Rename p r) = nub $ map ren $ ccsSucc defs p
                       where
                         f = renameFunction r
                         ren (a,p') = (f a, Rename p' r)

-- Rule CON
ccsSucc defs (Name pn) = case Map.lookup pn defs of
                           Just p  -> ccsSucc defs p
                           Nothing -> [] -- TODO raise error?


-- HML evaluation

hmlEvaluate :: ProcessDefinitions -> HMLFormula -> Process -> Bool
hmlEvaluate _ HMLTrue _ = True
hmlEvaluate _ HMLFalse _ = False
hmlEvaluate defs (HMLAnd f1 f2) p = (hmlEvaluate defs f1 p) && (hmlEvaluate defs f2 p)
hmlEvaluate defs (HMLOr  f1 f2) p = (hmlEvaluate defs f1 p) || (hmlEvaluate defs f2 p)
hmlEvaluate defs (HMLBox a f) p = all (hmlEvaluate defs f) (catMaybes $ map onlya $ ccsSucc defs p)
                                  where
                                    onlya (action,proc)
                                        | a == action = Just proc
                                        | otherwise = Nothing
hmlEvaluate defs (HMLDiamond a f) p = any (hmlEvaluate defs f) (catMaybes $ map onlya $ ccsSucc defs p)
                                      where
                                        onlya (action,proc)
                                            | a == action = Just proc
                                            | otherwise = Nothing


-- Helper functions

renameFunction :: Renaming -> Action -> Action
renameFunction r (Input a) = case Map.lookup a r of
                               Just a' -> Input a'
                               Nothing -> Input a
renameFunction r (Output a) = case Map.lookup a r of
                                Just a' -> Output a'
                                Nothing -> Output a
