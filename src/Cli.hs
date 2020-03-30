module Cli where

import qualified Data.Map as M
import System.Exit (exitSuccess)

import Game

data Command = Quit
             | Single Square
             | Pair Square Square
             | Remove Square

printTurn turn = putStrLn $ "Turn of " ++ show turn

-- Run a single loop of opening game
openingLoop :: (Int,Int) -> GameState -> IO()
openingLoop left game = do
  printSeparator
  renderGame game
  let turn = gameTurn game
  printTurn turn
  putStr "Where to place the next piece: "
  input <- getLine

  react (parseInput input) $ \cmd -> do
    case cmd of 
      Quit -> confirmQuit $ openingLoop left game

      -- Place a piece expects a single coordinate as input
      Single sq -> do

        -- makeOpeningMove checks that the move is valid
        case makeOpeningMove turn sq $ gameBoad game of
          Left err -> putStrLn err >> openingLoop left game
          
          Right newBoard -> do
            let newGameState = GameState (changeTurn turn) newBoard
                newleft = decrPiece turn left
            -- If there are still pieces left to be placed, continue opening
            -- otherwise change to midgame
            if newleft == (0,0)
              then midgameLoop newGameState
              else openingLoop newleft $ newGameState

      _ -> putStrLn "Invalid command during opening game" >>
           openingLoop left game

  where
    -- For an invalid command, keep the same state and rerun this function
    react = runCommand $ \err -> putStrLn err >> openingLoop left game

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

runCommand :: (String -> IO())
           -> Either String Command
           -> (Command -> IO())
           -> IO()           
runCommand onerror cmd handler = do
  case cmd of
    Left err -> onerror err
    Right command -> handler command
    
parseInput :: String -> Either String Command
parseInput inp = case words inp of
                   ["q"] -> Right Quit

                   [x] ->
                     Single <$> parseCoordinate x                     

                   [from, to] ->
                     Pair <$> parseCoordinate from <*> parseCoordinate to

                   _ -> Left "Command not understood"


printError err = putStrLn $ "Error! " ++ err
printSeparator = putStrLn "" >>
                 putStrLn "" >>
                 putStrLn "* * * * * * * * * * * * * * * * * * * *"

-- midgame

midgameLoop :: GameState -> IO()
midgameLoop game = do
  printSeparator
  renderGame game
  let turn = gameTurn game
  printTurn turn
  putStr "Which piece to move and where: "
  input <- getLine

  react (parseInput input) $ \cmd -> do
    case cmd of
      Quit -> confirmQuit $ midgameLoop game

      Pair from to -> do
        putStrLn $ "moving from " ++ show from ++ " to " ++ show to 

  where
    react = runCommand $ \err -> printError err >> midgameLoop game

renderGame :: GameState -> IO()
renderGame game = do
  putStrLn $ renderBoard $ gameBoard game
  putStrLn ""

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
    renderPiece ind = case lookupPiece (Square ind) brd of
                        Just p -> pieceAsChar p
                        Nothing -> '+'


pieceAsChar :: Piece -> Char
pieceAsChar X = 'X'
pieceAsChar O = 'O'

testBoard :: M.Map Square Piece
testBoard = M.fromList $ map (\(x,y) -> (Square x, y)) [(1,X), (5,O), (10,X), (12, X), (18, O)]
