module CCS where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List


-- AST for CCS

type Signal = String
type ProcessName = String
type Renaming = Map.Map Signal Signal
type Restriction = Set.Set Action

data Action
    = Input Signal
    | Output Signal
    | Tau
    deriving (Eq, Show, Ord)

data Process
    = Name ProcessName
    | Prefix Action Process
    | Choice [Process]
    | Parallel Process Process
    | Rename Process Renaming
    | Restrict Process Restriction
    deriving (Eq, Show)

-- Constant for the null process
nullProcess :: Process
nullProcess = Choice []

-- Symbol table
type ProcessDefinitions = Map.Map ProcessName Process


-- Complement of an action
complement :: Action -> Action
complement (Input s) = Output s
complement (Output s) = Input s
complement Tau = Tau

-- Get the signal of an actino
signal :: Action -> Signal
signal (Input s) = s
signal (Output s) = s
signal Tau = ""  -- Perhaps use a Maybe monad instead?

-- Reverse-Parser

showCCS :: Process -> String
showCCS (Name s) = s
showCCS (Prefix s p) = (showAction s) ++ "." ++ showCCS p
showCCS (Choice []) = "0"
showCCS (Choice l) = "(" ++ (List.intercalate " + " (map showCCS l)) ++ ")"
showCCS (Parallel p q) = "(" ++ (showCCS p) ++ " | " ++ (showCCS q) ++ ")"
showCCS (Rename p r) = (showCCS p) ++ "[" ++ (List.intercalate "," $ showRen r) ++ "]"
                       where
                         showRen = Map.foldWithKey (\k x acc -> (k ++ "/" ++ x):acc) []
showCCS (Restrict p r) = (showCCS p) ++ " \\ {" ++ (List.intercalate "," $ (map showAction $ Set.elems r)) ++ "}"

showAction :: Action -> String
showAction (Input s) = s
showAction (Output s) = '^':s
showAction Tau = "tau"



-- Non-recursive Hennessy-Milner logic

data HMLFormula
    = HMLTrue
    | HMLFalse
    | HMLAnd HMLFormula HMLFormula
    | HMLOr HMLFormula HMLFormula
    | HMLBox Action HMLFormula
    | HMLDiamond Action HMLFormula
    deriving (Eq,Show)

showHML :: HMLFormula -> String
showHML HMLTrue = "tt"
showHML HMLFalse = "ff"
showHML (HMLAnd a b) = concat ["(", showHML a, ") & (", showHML b, ")"]
showHML (HMLOr  a b) = concat ["(", showHML a, ") | (", showHML b, ")"]
showHML (HMLBox a f) = concat ["[", showAction a, "](", showHML f, ")"]
showHML (HMLDiamond a f) = concat ["<", showAction a, ">(", showHML f, ")"]