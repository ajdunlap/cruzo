module Data.Crossword
  where

#include "imports.h"

import Import
import Data.Map ( Map )
import qualified Data.Map as Map

type Coord = (Int,Int)
data Square a
  = Black
  | Unfilled
  | C a
  deriving (Eq,Show)

newtype Grid a = Grid (Map Coord (Square a)) deriving (Eq,Show)
