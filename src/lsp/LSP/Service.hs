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
import LSP.LSP
import ModuleExpander
import AST.AST

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

{- Server database that stores all compiled programs as a mapping 
from source names to programs 
-}
data ServerDatabase = ServerDatabase{
    programs :: ProgramTable
}

{-
-}
data ConnectionParams
    = STDIO
    | TCPServer Integer
    | TCPClient String Integer
    deriving (Show)

{- Start the Encore compiler in LSP mode. This will handle events from 
a client and handle them accordingly.

Param: ConnectionParams specifying mode and possibly host and port
-}
startServer :: ConnectionParams -> IO ()
startServer STDIO = do
    contents <- getContents
    let responseStream = handleClient contents

    hSetBuffering stdout NoBuffering
    hPutStr stdout responseStream

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

{- Lookup a ClassDecl in the server database by its name and the path to the file
where it was defined.

Param: Database to lookup in.
Param: Path to the file where the class was declared.
Param: Name of the class to lookup.

Return: Returns the looked up ClassDecl or Nothing if it was not found.
-}
lookupClass :: ServerDatabase -> FilePath -> String -> (Maybe ClassDecl)
lookupClass database sourceName className =
    case (Map.lookup sourceName (programs database)) of
        Just prog   ->  find (\cd -> (show $ cname cd) == className) (classes prog)
        Nothing     ->  Nothing
