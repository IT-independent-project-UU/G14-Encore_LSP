module LSP.Change (
    TextDocumentChange(..),
    TextDocumentContentChange(..),
    changeTextDocument,
    applyTextDocumentChange
)where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- LSP
import LSP.Producer
import LSP.Data.State
import LSP.Data.TextDocument

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

data TextDocumentChange = TextDocumentChange {
    tdcUri     :: String,
    tdcVersion :: Int,
    changes    :: [TextDocumentContentChange]
}

data TextDocumentContentChange = TextDocumentContentChange {
    range       :: ((Int, Int), (Int, Int)),
    rangeLength :: Int,
    text        :: String
}

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

changeTextDocument :: TextDocumentChange -> LSPState -> LSPState
changeTextDocument documentChange state@(LSPState textDocuments) =
    case Map.lookup (uri documentChange) textDocuments of
        Nothing -> state
        Just textDocument ->
            LSPState $ Map.insert (uri documentChange)
                                  (foldr (applyTextDocumentChange (uri documentChange)) state (changes documentChange))
                                  textDocuments

applyTextDocumentChange :: FilePath -> TextDocumentContentChange -> LSPState -> IO (LSPState)
applyTextDocumentChange uri change state = do
  case Map.lookup uri (textDocuments state) of
    Just doc -> do
      let newSource = applyTextChange (range change) (text change) (contents doc)
      newState <- produceAndUpdateState (tdcUri change) newSource state
      return (newState)
    Nothing -> return (state)

applyTextChange :: ((Int, Int), (Int, Int)) -> String -> String -> String
applyTextChange ((startLine, startChar), (endLine, endChar))
                replacement
                text
    = let textLines = split "\n" text
          startSegment = join "\n" $ take startLine textLines ++
                                     [take startChar $ textLines !! startLine]
          endSegment = join "\n" $ (drop endChar $ textLines !! endLine) :
                                   drop (endLine+1) textLines
      in startSegment ++ replacement ++ endSegment

-- ###################################################################### --
-- Section: Type classes
-- ###################################################################### --

instance TextDocumentURI TextDocumentChange where
    uri = tdcUri
instance TextDocumentVersion TextDocumentChange where
    version = tdcVersion

instance FromJSON TextDocumentChange where
    parseJSON = withObject "params" $ \o -> do
        identifier  <- o .: "textDocument"
        uri         <- identifier .: "uri"
        version     <- identifier .: "version"

        changes     <- o .: "contentChanges"

        return TextDocumentChange {
            tdcUri = uri,
            tdcVersion = version,
            changes = changes
        }

instance FromJSON TextDocumentContentChange where
    parseJSON = withObject "TextDocumentChange" $ \o -> do
        range       <- o .: "range"
        rangeStart  <- range .: "start"
        startLine   <- rangeStart .: "line"
        startChar   <- rangeStart .: "character"
        rangeEnd    <- range .: "end"
        endLine     <- rangeEnd .: "line"
        endChar     <- rangeEnd .: "character"
        rangeLength <- o .: "rangeLength"
        text        <- o .: "text"

        return TextDocumentContentChange {
            range = ((startLine, startChar), (endLine, endChar)),
            rangeLength = rangeLength,
            text = text
        }
