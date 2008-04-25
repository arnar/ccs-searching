module Console where

import CCS
import Parser
import Semantics

import Text.ParserCombinators.Parsec

import qualified Data.Map as Map

import System.Console.Shell
import System.Console.Shell.ShellMonad
import System.Console.Shell.Backend.Readline

import Control.Monad(foldM)

data CCSShellState
    = CCSShellState
      { procDefs :: ProcessDefinitions
      , histFile :: Maybe String
      }

initialShellState =
    CCSShellState
    { procDefs = Map.empty
    , histFile = Nothing
    }

ccsShell :: CCSShellState -> IO CCSShellState
ccsShell init = do
  let
      desc =
          (mkShellDescription commands evaluate)
          { historyFile = histFile init
          , commandStyle = CharPrefixCommands ':'
          , prompt = (\st -> return "ccs> ")
          , secondaryPrompt = Just (\st -> return "...> ")
          }
  runShell desc readlineBackend init

-- Cmd line parser and evaluation
data Command
    = DefineProcess ProcessName Process
    | ListDefinitions
    | ShowSuccessors Process
    | Parse Process
    | Test HMLFormula Process
    | Quit

cmdParser :: Parser [Command]
cmdParser = do whiteSpace
               let commands = 
                       [
                        do { symbol "def"
                           ; pn <- lexeme processName 
                           ; symbol "=" 
                           ; p <- process
                           ; return $ DefineProcess pn p
                           },
                        do { symbol "list"
                           ; return ListDefinitions
                           },
                        do { symbol "succ"
                           ; p <- process
                           ; return $ ShowSuccessors p
                           },
                        do { symbol "parse"
                           ; p <- process
                           ; return $ Parse p
                           },
                        do { symbol "test"
                           ; f <- hmlFormula
                           ; p <- process
                           ; return $ Test f p
                           },
                        do { (symbol "quit") <|> (symbol "exit")
                           ; return Quit
                           }
                       ]
               cmds <- sepEndBy (choice commands) semi
               eof
               return cmds
               
evaluate :: String -> Sh CCSShellState ()
evaluate "" = return ()
evaluate str = do case runParser (cmdParser) () "" str of
                    Left msg   -> 
                        if (lines $ show msg) !! 1 == "unexpected end of input" then
                            shellSpecial $ ShellContinueLine str
                        else
                            shellPutErrLn ("Parse error: " ++ (show msg))
                    Right cmds -> 
                        if null cmds then
                            shellPutErrLn "Unknown command"
                        else
                            mapM_ evalCommand cmds

evalCommand :: Command -> Sh CCSShellState ()

evalCommand (DefineProcess pn p) = 
    do st <- getShellSt
       putShellSt st{ procDefs = Map.insert pn p (procDefs st) }

evalCommand ListDefinitions =
    do st <- getShellSt
       shellPutStrLn "Process definitions"
       shellPutStrLn $ unlines $ map (uncurry printDef) $ Map.assocs (procDefs st)
       where
         printDef pn p = "  " ++ pn ++ " = " ++ (showCCS p)

evalCommand (ShowSuccessors p) =
    do st <- getShellSt
       shellPutStrLn $ unlines $ map (uncurry printStep) $ ccsSucc (procDefs st) p
       where
         printStep act proc = "--(" ++ (showAction act) ++ ")--> " ++ (showCCS proc)

evalCommand (Parse p) = shellPutStrLn $ showCCS p

evalCommand (Test f p) =
    do st <- getShellSt
       shellPutStrLn $ show (hmlEvaluate (procDefs st) f p)

evalCommand Quit =
    shellSpecial ShellExit

-- Auxiliary commands, accessible with prefix ":"
commands :: [ShellCommand (a)]
commands =  [cmd "echo" (echo) "print to output",
             exitCommand "quit"
            ]

echo :: String -> Sh (a) ()
echo s = shellPutStrLn ("You said: " ++ s)