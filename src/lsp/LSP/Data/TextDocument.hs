{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.TextDocument (
    TextDocument(..),
    uri,
    version,
    makeBlankTextDocument
) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Standard
import Data.Aeson
import Data.String.Utils (split, join)

-- Encore
import AST.AST

-- LSP
import LSP.Data.Error

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

data TextDocument = TextDocument {
    tdUri      :: String,
    languageId :: String,
    tdVersion  :: Int,
    contents   :: String,
    ast        :: Program,
    errors     :: [Error],
    warnings   :: [Error]
} deriving (Show)

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

makeBlankTextDocument :: String -> TextDocument
makeBlankTextDocument name = TextDocument{
    tdUri      = name,
    languageId = "encore",
    tdVersion  = 1,
    contents   = "",
    ast        = makeBlankAST name,
    errors     = [],
    warnings   = []
}

{-TODO move into AST.AST -}
{- Returns a blank program with a name -}
makeBlankAST :: FilePath -> Program
makeBlankAST name = Program{
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
-- Section: Type classes
-- ###################################################################### --

class TextDocumentURI a where
    uri :: a -> String
class TextDocumentVersion a where
    version :: a -> Int

instance TextDocumentURI TextDocument where
    uri = tdUri
instance TextDocumentVersion TextDocument where
    version = tdVersion

instance FromJSON TextDocument where
    parseJSON = withObject "params" $ \o -> do
        document <- o .: "textDocument"
        uri      <- document .: "uri"
        id       <- document .: "languageId"
        version  <- document .: "version"
        text     <- document .: "text"
        return TextDocument {
            tdUri = uri,
            languageId = id,
            tdVersion = version,
            contents = text,
            ast = makeBlankAST uri, -- TODO dont be blank
            errors = [],
            warnings = []
        }
