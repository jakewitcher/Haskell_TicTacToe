module Main where

import Control.Monad.IO.Class    (liftIO)
import Control.Monad.Trans.State (StateT(..), runStateT)
import Data.Char
import Data.Validation           (Validation(..))
import Lib
import System.Exit               (exitSuccess)
import Types

row = Row Nothing Nothing Nothing
newBoard = Board row row row

newGame :: IO ()
newGame = do 
    putStrLn "TIC TAC TOE"
    putStrLn ""
    putStrLn "How to play:"
    putStrLn "   1. Players take turns selecting where to place their marker"
    putStrLn "   2. Player 'X' will go first, followed by Player 'O'"
    putStrLn "   3. To place a marker, select a column and a row (ex. 'A1' or 'B3')"
    putStrLn "   4. First player with 3 markers in a row wins!"
    putStrLn ""

handleMoveErrors :: [MoveError] -> IO ()
handleMoveErrors errors = 
    let go ele = 
            case ele of 
                PositionUnavailable ->
                    "That position has already been selected"

                InvalidColumn c     ->
                    show c ++ " is not a valid column"

                InvalidRow c        -> 
                    show c ++ " is not a valid row"

                InvalidSelection    ->
                    "Invalid selection. Selection should be one column (A, B, or C)\
                    \ and one row (1, 2, or 3)"

    in sequence_ $ putStrLn . go <$> errors

handleUpdate :: Marker -> GameStatus -> StateT Board IO GameStatus
handleUpdate marker status = StateT $
    \s ->
        case status of 
            Continue   -> runStateT checkWinCondition s

            Error errs -> do 
                handleMoveErrors errs
                runStateT (startRound marker) s

            _          -> return (Continue, s)

handleWinCondition :: GameStatus -> StateT Board IO GameStatus
handleWinCondition status = StateT $
    \s -> 
        case status of 
            Win marker -> return (Win marker, s)
            _          -> runStateT checkStaleMate s

handleEndGame :: Board -> IO (GameStatus, Board)
handleEndGame board = do 
    putStrLn "Would you like to play again? [Y] [N]"
    response <- getLine

    case toUpper <$> response of 
        "Y" -> do 
            displayBoard newBoard
            runStateT (startRound X) newBoard
        "N" -> exitSuccess
        _   -> do 
            putStrLn "Invalid response, please try again"
            putStrLn ""
            handleEndGame board

gameWon :: Marker -> StateT Board IO GameStatus
gameWon marker = StateT $
    \s -> do
        displayBoard s
        putStrLn $ show marker ++ " won the game!"
        putStrLn ""
        handleEndGame s

gameStaleMate :: StateT Board IO GameStatus
gameStaleMate = StateT $
    \s -> do
        displayBoard s
        putStrLn "Game ended in a stalemate!"
        putStrLn ""
        handleEndGame s

handleEndRound :: Marker -> GameStatus -> StateT Board IO GameStatus
handleEndRound marker status =
    case status of 
        Win marker -> gameWon marker
        StaleMate  -> gameStaleMate
        _          -> StateT $
            \s -> do 
                displayBoard s
                runStateT (startRound (if marker == X then O else X)) s

playRound :: Marker -> (ColumnLabel, RowLabel) -> StateT Board IO GameStatus
playRound marker (col, row) = do
    updated      <- updateBoard row col marker
    winChecked   <- handleUpdate marker updated
    staleChecked <- handleWinCondition winChecked
    handleEndRound marker staleChecked

handleParsePlayerSelection :: Marker -> Validation [MoveError] (ColumnLabel, RowLabel) -> StateT Board IO GameStatus
handleParsePlayerSelection marker selection =
        case selection of 
            Failure errors -> do
                liftIO $ handleMoveErrors errors
                startRound marker

            Success sel    -> playRound marker sel

startRound :: Marker -> StateT Board IO GameStatus
startRound marker = do
        liftIO $ putStrLn "Select a position to place your marker:"
        selection <- liftIO getLine
        liftIO $ putStrLn ""
        handleParsePlayerSelection marker (parsePlayerSelection selection)

main :: IO ()
main = do 
    newGame
    putStrLn ""
    displayBoard newBoard
    putStrLn ""
    runStateT (startRound X) newBoard
    exitSuccess