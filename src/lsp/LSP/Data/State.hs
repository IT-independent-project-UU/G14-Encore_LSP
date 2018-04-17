module LSP.Data.State (
    LSPState,
    initial,
    addTextDocument,
    changeTextDocument
) where

import Data.Map as Map hiding (foldr)

import LSP.Data.TextDocument

data LSPState = LSPState {
    textDocuments :: Map String TextDocument
} deriving (Show)

initial :: LSPState
initial = LSPState {
    textDocuments = Map.empty
}

addTextDocument :: TextDocument -> LSPState -> LSPState
addTextDocument newDocument (LSPState textDocuments) =
    LSPState (Map.insert (uri newDocument) newDocument textDocuments)

changeTextDocument :: TextDocumentChange -> LSPState -> LSPState
changeTextDocument documentChange (LSPState textDocuments) =
    case Map.lookup (uri documentChange) textDocuments of
        Nothing -> LSPState textDocuments
        Just textDocument ->
            LSPState $ Map.insert (uri documentChange)
                                  (foldr applyTextDocumentChange textDocument (changes documentChange))
                                  textDocuments

