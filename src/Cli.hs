module Cli where

import qualified Data.Map as M
import System.Exit (exitSuccess)

import Game

data Input = Quit
           | Place Square
           | Move Square Square
           | Remove Square

-- Ask a next opening move, repeat until a succesful parse is obtained

askNextOpeningMove :: Piece -> IO(Input)
askNextOpeningMove turn = do
  putStrLn $ "Turn of " ++ show turn
  putStr "Where to place the next piece: "
  input <- getLine
  if input == "q"
    then return Quit
    else case parseCoordinate input of
           Right mv -> return $ Place mv
           Left err -> putStrLn err >>
                       askNextOpeningMove turn

openingLoop :: (Int,Int) -> GameState -> IO()
openingLoop left game = do
  renderGame game
  putStrLn ""
  let turn = gameTurn game
  input <- askNextOpeningMove turn

  case input of
    
    Quit -> confirmQuit $ openingLoop left game

    Place sq -> do  
      let newBoard = placePiece sq turn $ gameBoard game
          newGameState = GameState (changeTurn turn) (gameMode game) newBoard
          newleft = decrPiece turn left
      
      if newleft == (0,0)
        then midgameLoop newGameState
        else openingLoop newleft $ newGameState

  where
    decrPiece pcs (xleft, oleft) = if pcs == X
                                   then (xleft-1, oleft)
                                   else (xleft, oleft-1)
confirmQuit :: IO() -> IO()
confirmQuit backTo = do
  putStr "Are you sure you want to quit? [y/N]: "
  inp <- getLine
  case inp of
    "y" -> exitSuccess
    _   -> backTo

-- midgame

midgameLoop :: GameState -> IO()
midgameLoop game = do
  renderGame game
  putStrLn ""
  let turn = gameTurn game
  move <- askNextMidgameMove turn
  case move of
    Quit -> confirmQuit $ midgameLoop game
    Move from to -> do
      print "asd"


askNextMidgameMove :: Piece -> IO(Input)
askNextMidgameMove turn = do
  putStrLn $ "Turn of " ++ show turn
  putStr "Which piece to move and where: "
  input <- getLine
  if input == "q"
    then return Quit
    else  case parseMidgameMove input of
            Left error -> putStrLn error >> askNextMidgameMove turn
            Right (from, to) -> return $ Move from to

parseMidgameMove :: String -> Either String (Square, Square)
parseMidgameMove input = case words input of
                           [from, to] -> do
                             c1 <- parseCoordinate from
                             c2 <- parseCoordinate to
                             return (c1, c2)
                           _ -> Left "Please give coordinates separated by space"

renderGame :: GameState -> IO()
renderGame game = do
  putStrLn $ renderBoard $ gameBoard game

parseCoordinate :: String -> Either String Square
parseCoordinate x = case M.lookup x parseMap of
                      Just s ->
                        Right s
                      Nothing ->
                        Left $ "Input " ++ x ++ " is not a valid coordinate"

coordinates = ["a1","d1","g1",
               "b2","d2","f2",
               "c3","d3","e3",
               "a4","b4","c4","e4","f4","g4",
               "c5","d5","e5",
               "b6","d6","f6",
               "a7","d7","g7"]

parseMap :: M.Map String Square
parseMap = M.fromList $ zip coordinates (map Square [0..])


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


pieceAsChar :: Piece -> Char
pieceAsChar X = 'X'
pieceAsChar O = 'O'

testBoard :: M.Map Square Piece
testBoard = M.fromList $ map (\(x,y) -> (Square x, y)) [(1,X), (5,O), (10,X), (12, X), (18, O)]
