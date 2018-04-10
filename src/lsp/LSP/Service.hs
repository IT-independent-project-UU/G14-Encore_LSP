module LSP.Service  (
    startServer
) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Haskell imports
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.List

-- Encore imports
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


{- Start the Encore compiler in LSP mode. This will handle events from 
a client and handle them accordingly.

Param: String containing flags
-}
startServer :: String -> IO ()
startServer flags =
    do
        print ("Starting server (" ++ flags ++ ")")
        --forever runServer

{- Main run-loop of the Encore langauge server.
-}
runServer :: IO ()
runServer =
    do
        print "Running"

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
