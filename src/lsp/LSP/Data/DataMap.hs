module LSP.Data.DataMap where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Standard
import Data.Map as Map hiding (foldr)

-- LSP
import LSP.Data.Program
import LSP.Data.TextDocument

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

type LSPData = (Program, TextDocument)

type DataMap = Map String LSPData



-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --
