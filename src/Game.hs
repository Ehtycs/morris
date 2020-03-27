module Game where

import qualified Data.Map as M
import qualified Data.Vector as V

import Data.Foldable (foldl)

import Text.Printf

-- Board is a map indexed by Square coordinate 
type Board = M.Map Square Piece

-- Square is an Integer
newtype Square = Square {unSquare :: Int} deriving(Show, Ord, Eq)

-- The coordinates are calculated from top row, left to right

-- 0-----------1-----------2
-- |           |           |
-- |   3-------4-------5   |
-- |   |       |       |   |
-- |   |   6---7---8   |   |
-- |   |   |       |   |   |
-- 9--10--11      12--13--14
-- |   |   |       |   |   |
-- |   |   15--16--17  |   |
-- |   |       |       |   |
-- |   18------19-----20   |
-- |           |           |
-- 21----------22---------23


-- Two kinds of pieces
data Piece = X | O deriving(Show, Eq)

-- Morris has three game modes
-- 1. opening where pieces are placed on the board
-- 2. midgame where pieces are moved
-- 3. endgame where one or both of the players can hop around the board
data GameMode = Opening
              | Midgame
              | Endgame
              | Winner Piece deriving(Show)


data GameState = GameState { gameTurn :: Piece,
                             gameMode :: GameMode,
                             gameBoard :: Board
                           }

emptyState = GameState X Opening emptyBoard

emptyBoard = M.empty

changeTurn X = O
changeTurn O = X

placePiece :: Square -> Piece -> Board -> Board
placePiece = M.insert

-- Some helper definitions
-- TODO: remove these and write the move graph using ints
a1 = Square 0
d1 = Square 1
g1 = Square 2
b2 = Square 3
d2 = Square 4
f2 = Square 5
c3 = Square 6
d3 = Square 7
e3 = Square 8
a4 = Square 9
b4 = Square 10
c4 = Square 11
e4 = Square 12
f4 = Square 13
g4 = Square 14
c5 = Square 15
d5 = Square 16
e5 = Square 17
b6 = Square 18
d6 = Square 19
f6 = Square 20
a7 = Square 21
d7 = Square 22
g7 = Square 23

getPiece :: Square -> Board -> Maybe Piece
getPiece = M.lookup

countPieces :: Piece -> Board -> Int
countPieces psc = foldl (\acc x -> if x == psc then acc+1 else acc) 0

countXs :: Board -> Int
countXs = countPieces X

countOs :: Board -> Int
countOs = countPieces O

-- Legal moves graph in normal mode
legalMoves :: M.Map Square [Square]
legalMoves =
  M.fromList [(a1, [d1, a4]),
              (d1, [a1, d2, g1]),
              (g1, [d1, g4]),
              (b2, [d2, b4]),
              (d2, [d1, b2, d3, f2]),
              (f2, [d2, f4]),
              (c3, [d3, c4]),
              (d3, [d2, e3, c3]),
              (e3, [d3, e4]),
              (a4, [b4, a1, a7]),
              (b4, [a4, b2, c4, b6]),
              (c4, [c3, c5, b4]),
              (e4, [e3, f4, e5]),
              (f4, [e4, f2, g4, f6]),
              (g4, [g1, g7, f4]),
              (c5, [c4, d5]),
              (d5, [c5, e5, d6]),
              (e5, [e4, d5]),
              (b6, [b4, d6]),
              (d6, [b6, d5, f6, d7]),
              (f6, [f4, d6]),
              (a7, [a4, d7]),
              (d7, [a7, d6, g7]),
              (g7, [g4, d7])]

-- Check if a move is legal in normal mode
-- doesn't check if board is already occupied
isLegalNormalMode :: Square -> Square -> Bool
isLegalNormalMode frm to = case M.lookup frm legalMoves of
                             Just lst -> elem to lst
                             -- Should never happen
                             Nothing  -> error "Square doesn't exist!"

-- Check if a square is empty
isEmpty :: Board -> Square -> Bool
isEmpty brd sqr = case M.lookup sqr brd of
                    Just _ -> False
                    Nothing -> True


        


