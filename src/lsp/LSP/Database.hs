module LSP.Database (
    DBProgramTable,
    DBProgram,
    ServerDatabase,
    makeDBProgram,
    getDBProgram,
    convertFromProgramTable,
    convertToProgramTable,
    hasProgramTableError,

    dumpDBProgramTable
) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Standard
import qualified Data.Map.Strict as Map
import Data.List

-- Encore imports
import ModuleExpander
import AST.AST
import Typechecker.TypeError


-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

type DBProgramTable = Map.Map FilePath DBProgram

data DBProgram = DBProgram{
    program     :: Program,
    version     :: Int,
    errors      :: [TCError],
    warnings    :: [TCWarning]
}

{- Server database that stores all compiled programs as a mapping 
from source names to programs 
-}
data ServerDatabase = ServerDatabase{
    programs :: DBProgramTable
}

-- ###################################################################### --
-- Section: Construction
-- ###################################################################### --

createDatabase :: ServerDatabase
createDatabase = ServerDatabase{programs = Map.empty}

makeDBProgram :: Program -> [TCError] -> [TCWarning] -> DBProgram
makeDBProgram _program _errors _warnings =
     DBProgram{program = _program, version = 0, errors = _errors, warnings = _warnings}

getDBProgram :: DBProgram -> Program
getDBProgram _program = (program _program)

convertFromProgramTable :: ProgramTable -> DBProgramTable
convertFromProgramTable table = 
    fmap (_convertFromProgramSingle) table
    where 
        _convertFromProgramSingle :: Program -> DBProgram
        _convertFromProgramSingle program = (makeDBProgram program [] [])

convertToProgramTable :: DBProgramTable -> ProgramTable
convertToProgramTable table = fmap (getDBProgram) table

-- ###################################################################### --
-- Section: Insertion and lookup
-- ###################################################################### --

hasProgramTableError :: DBProgramTable -> Bool
hasProgramTableError programTable =
    True `elem` (fmap (_hasProgramError) programTable)
    where 
        _hasProgramError program = length (errors program) /= 0 

insertProgramTable :: ServerDatabase -> ProgramTable -> IO ()
insertProgramTable db pt = do
    return ()

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
        Just prog   ->  find (\cd -> (show $ cname cd) == className) (classes (program prog))
        Nothing     ->  Nothing


-- ###################################################################### --
-- Section: Debug
-- ###################################################################### --

dumpDBProgramTable :: DBProgramTable -> IO ()
dumpDBProgramTable programTable = do
    mapM_ (_dump) programTable
    where 
        _dump program = do
            mapM_ (print) (warnings program)
            mapM_ (print) (errors program)