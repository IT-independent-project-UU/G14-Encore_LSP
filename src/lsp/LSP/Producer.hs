{-# LANGUAGE TemplateHaskell #-}

module LSP.Producer (
    produceAndUpdateState
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
import LSP.Data.TextDocument
import LSP.Data.Error
import LSP.Data.State

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

produceAndUpdateState :: FilePath -> String -> LSPState -> IO (LSPState)
produceAndUpdateState sourceName source state = do
    -- Parse program to produce AST
    (ast, error) <- case parseEncoreProgram sourceName source of
        Right ast   -> return (ast, Nothing)
        Left error  -> return ((makeBlankAST sourceName), Just error)

    case error of
        Just e -> do
            --print "Failed to parse program"
            let error = fromParsecError e
            let newState = (addTextDocument (makeBlankTextDocument sourceName) state)
            return (newState)
        Nothing -> do
            -- Build program table from AST
            programTable <- buildProgramTable preludePaths preludePaths ast
            let desugaredTable = fmap desugarProgram programTable

            -- Convert the desugared table into a LSPState
            let lspState = convertFromProgramTable desugaredTable

            -- Precheck and typecheck the table
            precheckedTable <- producerPrecheck lspState
            typecheckedTable <- producerTypecheck precheckedTable
            return (typecheckedTable)

updateTextDocumentInState :: LSPState -> TextDocument -> String -> IO (LSPState)
updateTextDocumentInState state document source = do
    case Map.loopup (tdUri document) (textDocuments state) of
      Just doc -> do
        let newDoc = TextDocument {
              tdUri = tdUri doc,

                                  }


convertFromProgramTable :: ProgramTable -> TextDocumentMap
convertFromProgramTable table =
    fmap (_convertFromProgramSingle) table
    where
        _convertFromProgramSingle :: Program -> TextDocument
        _convertFromProgramSingle program = (makeBlankTextDocument (source program))

-- ###################################################################### --
-- Section: Type checking
-- ###################################################################### --

producerPrecheckProgram :: (Map.Map FilePath LookupTable) -> TextDocument -> IO (TextDocument)
producerPrecheckProgram lookupTable program = do
    case precheckProgram lookupTable (convertToProgram program) of
        (Right newProgram, warnings)    -> return $ makeDBProgram newProgram [] (fromTCWarnings warnings)
        (Left error, warnings)          -> return $ makeDBProgram blankProgram (fromTCErrors [error]) (fromTCWarnings warnings)

producerPrecheck :: LSPState -> IO (LSPState)
producerPrecheck programTable = do
    let lookupTable = fmap buildLookupTable (convertToProgramTable programTable)
    mapM (_producerPrecheck lookupTable) programTable
    where
        _producerPrecheck lookupTable program = do
            (precheckedProgram) <- (producerPrecheckProgram lookupTable program)
            return (precheckedProgram)

producerTypecheckProgram :: (Map.Map FilePath LookupTable) -> TextDocument -> IO (TextDocument)
producerTypecheckProgram lookupTable program = do
    case typecheckProgram lookupTable (convertToProgram program) of
        (Right (env, newProgram), warnings)     -> return $ makeDBProgram newProgram [] (fromTCWarnings warnings)
        (Left error, warnings)   -> return $ makeDBProgram blankProgram (fromTCErrors [error]) (fromTCWarnings warnings)

producerTypecheck :: LSPState -> IO (LSPState)
producerTypecheck programTable = do
    let lookupTable = fmap buildLookupTable (convertToProgramTable programTable)
    mapM (_producerTypecheck lookupTable) programTable
    where
        _producerTypecheck lookupTable program = do
            (typecheckedProgram) <- (producerTypecheckProgram lookupTable program)
            return (typecheckedProgram)
