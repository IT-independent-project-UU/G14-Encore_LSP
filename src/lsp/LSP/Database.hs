{-# LANGUAGE TemplateHaskell #-}


module LSP.Database (
    DBProgramTable,
    DBProgram,
    ServerDatabase,
    makeDatabase,
    makeDBProgram,
    convertToProgram,
    convertFromProgramTable,
    convertToProgramTable,
    hasProgramTableError,
    fromTCErrors,
    fromTCWarnings,
    fromParsecError,
    fromErrorMessage,
    updateProgramTable,
    lookupClass,
    getDatabasePrograms,

    dumpDBProgramTable
) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Library
import Text.Megaparsec

-- Standard
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE(head)
import Data.List
import Control.Monad

-- Encore imports
import ModuleExpander
import AST.AST
import AST.Meta(Position(SingletonPos, RangePos))
import Typechecker.TypeError
import Typechecker.Environment



-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

{- Alias for a map from FilePath to a DBProgram -}
type DBProgramTable = Map.Map FilePath DBProgram

{- Position tuple  -}
type DBPosition = ((Int, Int), (Int, Int))

{- Error datastructure. Can represent a variety of different error types -}
data DBError = DBError{
    message     :: String,      -- Error message string
    position    :: DBPosition,  -- Start and end position
    warning     :: Bool         -- Whether the error is actually a warning
} deriving (Show)

{- Datastructure that represents a single compilation unit in Encore. This 
    mostly mimics the Program structure found in AST.hs, however it also 
    contains some extra information useful to the LSP implementation.
-}
data DBProgram = DBProgram{
    program     :: Program,     -- Program (AST.hs)
    version     :: Int,         -- Version of the file
    errors      :: [DBError],   -- List of current errors in unit
    warnings    :: [DBError]    -- List of warnings in unit
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

makeDatabase :: ServerDatabase
makeDatabase = ServerDatabase{programs = Map.empty}

makeDBProgram :: Program -> [DBError] -> [DBError] -> DBProgram
makeDBProgram _program _errors _warnings =
     DBProgram{program = _program, version = 0, errors = _errors, warnings = _warnings}

convertToProgram :: DBProgram -> Program
convertToProgram _program = (program _program)

convertFromProgramTable :: ProgramTable -> DBProgramTable
convertFromProgramTable table = 
    fmap (_convertFromProgramSingle) table
    where 
        _convertFromProgramSingle :: Program -> DBProgram
        _convertFromProgramSingle program = (makeDBProgram program [] [])

convertToProgramTable :: DBProgramTable -> ProgramTable
convertToProgramTable table = fmap (convertToProgram) table

makeLookupTable :: DBProgramTable -> (Map.Map FilePath LookupTable)
makeLookupTable programTable = fmap (\x -> buildLookupTable (program x)) programTable


-- ###################################################################### --
-- Section: Accessors and mutators
-- ###################################################################### --

getProgramPath :: DBProgram -> String
getProgramPath _program = (source (program _program))

getDatabasePrograms :: ServerDatabase -> DBProgramTable
getDatabasePrograms database = (programs database)

-- ###################################################################### --
-- Section: Error handling
-- ###################################################################### --

extractTCErrorPosition :: TCError -> DBPosition
extractTCErrorPosition error@(TCError errorType backtrace) =
    case fst (head backtrace) of
        (AST.Meta.SingletonPos startPos) -> 
            ((fromIntegral(unPos $ sourceLine $ startPos), fromIntegral(unPos $ sourceColumn $ startPos)),
            (fromIntegral(unPos $ sourceLine $ startPos), fromIntegral(unPos $ sourceColumn $ startPos)))
        (AST.Meta.RangePos startPos endPos) -> 
            ((fromIntegral(unPos $ sourceLine $ startPos), fromIntegral(unPos $ sourceColumn $ startPos)),
            (fromIntegral(unPos $ sourceLine $ endPos), fromIntegral(unPos $ sourceColumn $ endPos)))

extractTCWarningPosition :: TCWarning -> DBPosition
extractTCWarningPosition warning = ((0,0), (0,0))

fromTCError :: TCError -> DBError
fromTCError error = 
    DBError{message = show error, position = extractTCErrorPosition error, warning = False}

fromTCErrors :: [TCError] -> [DBError]
fromTCErrors errors = fmap (fromTCError) errors

fromTCWarning :: TCWarning -> DBError
fromTCWarning warning = 
    DBError{message = show warning, position = extractTCWarningPosition warning, warning = True}

fromTCWarnings :: [TCWarning] -> [DBError]
fromTCWarnings warnings = fmap (fromTCWarning) warnings

fromParsecError :: (ParseError Char Text.Megaparsec.Dec) -> DBError
fromParsecError error =
    DBError{
        message = show error, 
        position = ((fromIntegral(unPos $ sourceLine $ NE.head $ errorPos error), fromIntegral(unPos $ sourceColumn $ NE.head $ errorPos error)), 
                    (fromIntegral(unPos $ sourceLine $ NE.head $ errorPos error), fromIntegral(unPos $ sourceColumn $ NE.head $ errorPos error))), 
        warning = False
    }

{- Construct a DBError from a message, position and whether it is actually a 
    warning

    Param: Error message
    Param: Position (row, column)
    Param: Whether or not the error is actually a warning
-}
fromErrorMessage :: String -> DBPosition -> Bool -> DBError
fromErrorMessage _message _position _warning = 
    DBError{message = _message, position = _position, warning = _warning}

hasProgramTableError :: DBProgramTable -> Bool
hasProgramTableError programTable =
    True `elem` (fmap (_hasProgramError) programTable)
    where 
        _hasProgramError program = length (errors program) /= 0     

-- ###################################################################### --
-- Section: Insertion and lookup
-- ###################################################################### --

updateProgramTable :: ServerDatabase -> DBProgramTable -> ServerDatabase
updateProgramTable database newTable = ServerDatabase {
    programs = Map.unionWith (\x y -> y) (programs database) (newTable)
}
    
    
    

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