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

{- Start the Encore compiler in LSP mode. This will handle events from 
a client and handle them accordingly.

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

Param: ConnectionParams specifying mode and possibly host and port
-}
startServer :: ConnectionParams -> IO ()
startServer STDIO = do
    progTable <- produceProgramFromSource ":srv:"
        ("active class Main\n" ++
        "   def main(): unit\n" ++
        "       println(\"Hello embedded source\")\n" ++
        "   end\n" ++
        "end")

    let db = updateProgramTable makeDatabase progTable

    print $ "Size: " ++ show (Map.size $ getDatabasePrograms db)

    case lookupClass db ":srv:" "Main" of
        Just cd -> do
            print "Found class"
        Nothing -> do
            print "Did not found class"

    --dumpDBProgramTable progTable

    --case hasProgramTableError progTable of
    --    True -> print "Error haha!!"
    --    False -> return ()

    --print $ "Size: " ++ show (Map.size progTable)

    --case (lookupClass progTable)


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

        contents <- hGetContents client

        forkIO $ do let responseStream = handleClient contents
                    hPutStr client responseStream

startServer (TCPClient host port) = do
    putStrLn $ "connecting to " ++ (show host) ++ " :" ++ (show port)

    sock <- connectTo host $ PortNumber $ fromInteger port

    contents <- hGetContents sock
    let responseStream = handleClient contents

    hSetBuffering sock NoBuffering
    hPutStr sock responseStream