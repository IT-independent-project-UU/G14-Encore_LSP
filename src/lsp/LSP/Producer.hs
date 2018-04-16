{-# LANGUAGE TemplateHaskell #-}

module LSP.Producer (
    produceProgramFromSource
) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Library
import Text.Megaparsec

-- Standard
import qualified Data.List.NonEmpty as NE(head)
import Language.Haskell.TH
import System.Environment
import System.Exit
import qualified Data.Map.Strict as Map
import Control.Monad

-- Encore
import Parser.Parser
import AST.AST
import AST.Desugarer
import AST.PrettyPrinter
import ModuleExpander
import Typechecker.Environment
import Typechecker.Prechecker(precheckProgram)
import Typechecker.Typechecker(typecheckProgram, checkForMainClass)
import Typechecker.TypeError
import Utils

-- LSP
import LSP.Database

-- ###################################################################### --
-- Section: Support
-- ###################################################################### --

-- the following line of code resolves the standard path at compile time using Template Haskell
standardLibLocation = $(stringE . init =<< runIO (System.Environment.getEnv "ENCORE_MODULES" ))

preludePaths =
    [standardLibLocation ++ "/standard", standardLibLocation ++ "/prototype"]

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

{-
produceProgramFromFile :: [FilePath] -> String -> IO ProgramTable
produceProgramFromFile filePaths source = do
    let sourceName = head filePaths
-}



produceProgramFromSource :: String -> String -> IO (DBProgramTable) 
produceProgramFromSource sourceName source = do
    -- Parse program to produce AST
    (ast, error) <- case parseEncoreProgram sourceName source of
        Right ast   -> return (ast, Nothing)
        Left error  -> return (blankProgram, Just error)

    case error of
        Just e -> do 
            --print "Failed to parse program"
            let dbError = fromParsecError e
            let dbProgram = (makeDBProgram (blankNamedProgram sourceName) [dbError] [])
            let dbTable = (Map.insert sourceName dbProgram (Map.empty :: DBProgramTable))
            return (dbTable)
        Nothing -> do
            -- Build program table from AST
            programTable <- buildProgramTable preludePaths preludePaths ast
            let desugaredTable = fmap desugarProgram programTable

            -- Convert the desugared table into a DBProgramTable
            let dbTable = convertFromProgramTable desugaredTable

            -- Precheck and typecheck the table
            precheckedTable <- producerPrecheck dbTable
            typecheckedTable <- producerTypecheck precheckedTable
            return (typecheckedTable)


{- Returns a blank program  -}
blankProgram :: Program
blankProgram = Program{
    source = "",
    moduledecl = NoModule,
    etl = [],
    imports = [],
    typedefs = [],
    functions = [],
    traits = [],
    classes = []
}

{- Returns a blank program with a name -}
blankNamedProgram :: String -> Program
blankNamedProgram name = Program{
    source = name,
    moduledecl = NoModule,
    etl = [],
    imports = [],
    typedefs = [],
    functions = [],
    traits = [],
    classes = []
}

-- ###################################################################### --
-- Section: Type checking
-- ###################################################################### --

producerPrecheckProgram :: (Map.Map FilePath LookupTable) -> DBProgram -> IO (DBProgram)
producerPrecheckProgram lookupTable program = do
    case precheckProgram lookupTable (getDBProgram program) of
        (Right newProgram, warnings)    -> return $ makeDBProgram newProgram [] (fromTCWarnings warnings)
        (Left error, warnings)          -> return $ makeDBProgram  blankProgram (fromTCErrors [error]) (fromTCWarnings warnings)

producerPrecheck :: DBProgramTable -> IO (DBProgramTable)
producerPrecheck programTable = do
    let lookupTable = fmap buildLookupTable (convertToProgramTable programTable)
    mapM (_producerPrecheck lookupTable) programTable
    where
        _producerPrecheck lookupTable program = do
            (precheckedProgram) <- (producerPrecheckProgram lookupTable program)
            return (precheckedProgram)

producerTypecheckProgram :: (Map.Map FilePath LookupTable) -> DBProgram -> IO (DBProgram)
producerTypecheckProgram lookupTable program = do
    case typecheckProgram lookupTable (getDBProgram program) of
        (Right (env, newProgram), warnings)     -> return $ makeDBProgram newProgram [] (fromTCWarnings warnings)
        (Left error, warnings)   -> return $ makeDBProgram blankProgram (fromTCErrors [error]) (fromTCWarnings warnings)

producerTypecheck :: DBProgramTable -> IO (DBProgramTable)
producerTypecheck programTable = do
    let lookupTable = fmap buildLookupTable (convertToProgramTable programTable)
    mapM (_producerTypecheck lookupTable) programTable
    where
        _producerTypecheck lookupTable program = do
            (typecheckedProgram) <- (producerTypecheckProgram lookupTable program)
            return (typecheckedProgram)