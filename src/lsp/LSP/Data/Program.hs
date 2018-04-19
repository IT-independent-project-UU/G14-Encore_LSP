 module LSP.Data.Program where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Standard
import System.Exit

-- Encore
import qualified AST.AST as AST
import qualified AST.Meta as ASTMeta

-- LSP
import LSP.Data.Error
import qualified LSP.Data.Position as LSP

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

data Program = Program {
  ast :: AST.Program,
  errors :: [Error],
  warnings :: [Error]
} deriving (Show)

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

makeBlankProgram :: FilePath -> Program
makeBlankProgram path = Program {
  ast = makeBlankAST path,
  errors = [],
  warnings = []
}

makeBlankAST :: FilePath -> AST.Program
makeBlankAST path = AST.Program {
  AST.source = path,
  AST.moduledecl = AST.NoModule,
  AST.etl = [],
  AST.imports = [],
  AST.typedefs = [],
  AST.functions = [],
  AST.traits = [],
  AST.classes = []
}

findNodeFromPosition :: LSP.Position -> Program -> IO ()
findNodeFromPosition range program = do
  thing <- (findNodeFromFunction range (AST.functions (ast program)))
  case thing of
    Just s -> print $ s
    Nothing -> die "reached end"

handleSingletonPos :: String
handleSingletonPos = "############ BAD ################"

findNodeFromFunction :: LSP.Position -> [AST.Function] -> IO (Maybe String)
findNodeFromFunction _ [] = return (Nothing)
findNodeFromFunction (row, col) (x:xs) =
  case (ASTMeta.getPos (AST.funmeta x)) of
    ASTMeta.SingletonPos _ -> return (Just handleSingletonPos)
    ASTMeta.RangePos start end -> return (Nothing)

findNodeFromParamDecls :: LSP.Position -> [AST.ParamDecl] -> Maybe String
findNodeFromParamDecls _ [] = Nothing
findNodeFromParamDecls range (x:xs) =
  case (ASTMeta.getPos (AST.pmeta x)) of
    ASTMeta.SingletonPos _ -> Just handleSingletonPos
    ASTMeta.RangePos start end -> Just $ "Range: " ++ show (start, end)
