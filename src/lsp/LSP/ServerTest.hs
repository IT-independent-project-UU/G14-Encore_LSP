module LSP.ServerTest (testServer) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Haskell imports
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import qualified Data.Map.Strict as Map
import Data.List
import Network
import System.IO

-- Encore imports
import ModuleExpander
import AST.AST

-- LSP imports
import LSP.LSP
import LSP.Producer

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

getProgramFromStdioAux :: String -> IO (String)
getProgramFromStdioAux input
  | input == ['\n'] = return ['\n']
  | otherwise = do
      a <- getProgramFromStdio
      return $ input ++ ['\n'] ++ a

getProgramFromStdio :: IO (String)
getProgramFromStdio = do
  ok <- hIsEOF stdin
  if ok then do
    return []
  else do
    input <- getLine
    getProgramFromStdioAux input

testServer :: IO ()
testServer = do
  -- run like this: cat playground/main.enc | encorec -s stdio
    program <- getProgramFromStdio
    progTable <- produceProgramFromSource ":srv:" program

    let db = updateProgramTable makeDatabase progTable

    print $ "Size: " ++ show (Map.size $ getDatabasePrograms db)

    case lookupClass db ":srv:" "Main" of
        Just cd -> do
            print "Found class"
        Nothing -> do
            print "Did not found class"
    return ()
