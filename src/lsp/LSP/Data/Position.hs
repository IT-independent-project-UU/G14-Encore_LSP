module LSP.Data.Position (
  Position,
  Range,
  inRange,
  fromSourcePos,
  fromSourcePosRange
) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Library
import Text.Megaparsec(SourcePos,unPos,sourceLine,sourceColumn)

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

{-  -}
type Position = (Int, Int)

{-  -}
type Range = (Position, Position)

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

{-  -}
inRange :: Position -> Range -> Bool
inRange pos (start, end)
  | fst pos > fst start && fst pos < fst end = True
  | fst pos == fst start && fst pos < fst end && snd pos >= snd start = True
  | fst pos > fst start && fst pos == fst end && snd pos <= snd end = True
  | fst pos == fst start && fst pos == fst end && snd pos >= snd start && snd pos <= snd end = True
  | True = False -- sorry, had to do it

{-  -}
fromSourcePos :: SourcePos -> Position
fromSourcePos pos = (fromIntegral (unPos (sourceLine pos)), fromIntegral (unPos (sourceColumn pos)))

{-  -}
fromSourcePosRange :: SourcePos -> SourcePos -> Range
fromSourcePosRange from to = (fromSourcePos from, fromSourcePos to)