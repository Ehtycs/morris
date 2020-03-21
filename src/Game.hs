module Game where

import qualified Data.Map as M
import qualified Data.Vector as V

import Text.Printf

type Board = M.Map Square Piece

newtype Square = Square {unSquare :: Int} deriving(Show, Ord, Eq)

data Piece = X | O deriving(Show)

data GameMode = Opening | Midgame | Endgame deriving(Show)

data GameState = GameState { gameTurn :: Piece,
                             gameMode :: GameMode,
                             gameBoard :: Board
                           }

emptyState = GameState X Opening emptyBoard

emptyBoard = M.empty

gameLoop :: GameState -> IO()
gameLoop game = do
  renderGame game
  putStrLn ""
  let turn = gameTurn game
  move <- askNextOpeningMove turn
  let newBoard = placePiece move turn $ gameBoard game
  gameLoop $ GameState (changeTurn turn) (gameMode game) newBoard

changeTurn X = O
changeTurn O = X

placePiece = M.insert 

askNextOpeningMove :: Piece -> IO(Square)
askNextOpeningMove turn = do
  putStrLn $ "Turn of " ++ show turn
  putStr "Where to place the next piece: "
  input <- getLine
  case parseSquare input of
    Just mv -> return mv
    Nothing -> putStrLn "Invalid command. Try again" >>
               askNextOpeningMove turn
  

parseSquare str = Just $ Square $ read str

renderGame :: GameState -> IO()
renderGame game = do
  putStrLn $ renderBoard $ gameBoard game

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

boardTemplate = "   a   b   c   d   e   f   g\n" ++ 
                "                            \n" ++
                "1  +-----------+-----------+\n" ++
                "   |           |           |\n" ++
                "2  |   +-------+-------+   |\n" ++
                "   |   |       |       |   |\n" ++
                "3  |   |   +---+---+   |   |\n" ++
                "   |   |   |       |   |   |\n" ++
                "4  +---+---+       +---+---+\n" ++
                "   |   |   |       |   |   |\n" ++
                "5  |   |   +---+---+   |   |\n" ++
                "   |   |       |       |   |\n" ++
                "6  |   +-------+-------+   |\n" ++
                "   |           |           |\n" ++
                "7  +-----------+-----------+\n"

renderBoard brd = fst $ foldl foldfun ("", 0) boardTemplate
  where
    foldfun (str, count) '+' = (str ++ [renderPiece count], count+1)
    foldfun (str, count) x  = (str ++ [x], count)
    renderPiece ind = case getPiece (Square ind) brd of
                        Just p -> pieceAsChar p
                        Nothing -> '+'

getPiece = M.lookup

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
          
isLegalNormalMode :: Square -> Square -> Bool
isLegalNormalMode frm to = case M.lookup frm legalMoves of
                             Just lst -> elem to lst
                             -- Should never happen
                             Nothing  -> error "Square doesn't exist!"

isEmpty :: Board -> Square -> Bool
isEmpty brd sqr = case M.lookup sqr brd of
                    Just _ -> False
                    Nothing -> True

pieceAsChar :: Piece -> Char
pieceAsChar X = 'X'
pieceAsChar O = 'O'

testBoard :: M.Map Square Piece
testBoard = M.fromList $ map (\(x,y) -> (Square x, y)) [(1,X), (5,O), (10,X), (12, X), (18, O)]

