module LSP.Service  (
    startServer,
    ConnectionParams(..)
) where

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
import LSP.Database

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

{-
-}
data ConnectionParams
    = STDIO
    | TCPServer Integer
    | TCPClient String Integer
    deriving (Show)

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

{- Start the Encore compiler in LSP mode. This will handle events from
a client and handle them accordingly.
Param: ConnectionParams specifying mode and possibly host and port
-}
startServer :: ConnectionParams -> IO ()
startServer STDIO = do
    -- run like this: cat playground/main.enc | encorec -s stdio
    program <- getProgramFromStdio
    progTable <- produceProgramFromSource ":srv:" program
    {-
        ("active class Main\n" ++
        "   def main(): unit\n" ++
        "       println(\"Hello embedded source\")\n" ++
        "   end\n" ++
        "end")
    -}

    dumpDBProgramTable progTable

    case hasProgramTableError progTable of
        True -> print "Error haha!!"
        False -> return ()

    --print $ "Size: " ++ show (Map.size progTable)

    return ()

    {-contents <- getContents
    let responseStream = handleClient contents

    hSetBuffering stdout NoBuffering
    hPutStr stdout responseStream-}

startServer (TCPServer port) = do
    sock <- listenOn $ PortNumber $ fromInteger port
    putStrLn $ "listening on " ++ (show port)
    forever $ do
        (client, addr, _) <- accept sock
        putStrLn $ "connection from " ++ (show addr)

        forkIO $ do contents <- hGetContents client
                    let responseStream = handleClient contents
                    hPutStr client responseStream

startServer (TCPClient host port) = do
    putStrLn $ "connecting to " ++ (show host) ++ " :" ++ (show port)

    sock <- connectTo host $ PortNumber $ fromInteger port

    contents <- hGetContents sock
    let responseStream = handleClient contents

    hSetBuffering sock NoBuffering
    hPutStr sock responseStream
