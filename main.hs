module Main where

import Console(ccsShell, initialShellState)

main :: IO ()
main = do
  ccsShell initialShellState
  return ()