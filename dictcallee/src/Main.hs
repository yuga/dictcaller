module Main (
  main
) where

import qualified Server
--import System.Environment (getArgs)

main :: IO ()
main = Server.run 3000
