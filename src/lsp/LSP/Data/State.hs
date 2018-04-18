module LSP.Data.State (
    TextDocumentMap,
    LSPState,
    initial,
    addTextDocument
) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Standard
import Data.Map as Map hiding (foldr)

-- LSP
import LSP.Data.TextDocument

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

type TextDocumentMap = Map String TextDocument

data LSPState = LSPState {
    textDocuments :: TextDocumentMap
} deriving (Show)

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

initial :: LSPState
initial = LSPState {
    textDocuments = Map.empty
}

addTextDocument :: TextDocument -> LSPState -> LSPState
addTextDocument newDocument (LSPState textDocuments) =
    LSPState (Map.insert (uri newDocument) newDocument textDocuments)
