 module LSP.Data.Program where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Encore
import qualified AST.AST as AST

-- LSP
import LSP.Data.Error

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

data Program = Program {
  program :: AST.Program,
  errors :: [Error],
  warnings :: [Error]
} deriving (Show)

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

makeProgram :: FilePath -> Program
makeProgram path = Program {
  program = makeBlankAST path,
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
