module Lib where

import Control.Applicative       (liftA2)
import Control.Monad.Trans.State (StateT(..))
import Data.Char
import Data.Maybe                (isJust, maybe)
import Data.Validation           (Validation(..))
import Types

updateMarker :: ColumnLabel -> Marker -> Row -> Validation [MoveError] Row 
updateMarker col marker (Row a b c) =
    case col of 
        A -> 
            case a of 
                Nothing -> Success $ Row (Just marker) b c 
                _       -> Failure [PositionUnavailable]
        B -> 
            case b of 
                Nothing -> Success $ Row a (Just marker) c 
                _       -> Failure [PositionUnavailable]
        C -> 
            case c of 
                Nothing -> Success $ Row a b (Just marker)
                _       -> Failure [PositionUnavailable]

updateBoard :: RowLabel -> ColumnLabel -> Marker -> StateT Board IO GameStatus
updateBoard row col marker = StateT $
    \(Board r1 r2 r3) ->
        case row of 
            One   ->  
                case updateMarker col marker r1 of 
                    Success r1' -> return (Continue, Board r1' r2 r3)
                    Failure err -> return (Error err, Board r1 r2 r3)
            Two   -> 
                case updateMarker col marker r2 of 
                    Success r2' -> return (Continue, Board r1 r2' r3)
                    Failure err -> return (Error err, Board r1 r2 r3)  
            Three -> 
                case updateMarker col marker r3 of 
                    Success r3' -> return (Continue, Board r1 r2 r3')
                    Failure err -> return (Error err, Board r1 r2 r3)

tryHorizontalWin :: Row -> Row -> Row -> Maybe Marker
tryHorizontalWin (Row a1 b1 c1) (Row a2 b2 c2) (Row a3 b3 c3)
    | a1 == b1 && b1 == c1 = a1 
    | a2 == b2 && b2 == c2 = a2
    | a3 == b3 && b3 == c3 = a3
    | otherwise            = Nothing

tryVerticalWin :: Row -> Row -> Row -> Maybe Marker 
tryVerticalWin (Row a1 b1 c1) (Row a2 b2 c2) (Row a3 b3 c3)
    | a1 == a2 && a2 == a3 = a1
    | b1 == b2 && b2 == b3 = b1
    | c1 == c2 && c2 == c3 = c1
    | otherwise            = Nothing

tryDiagonalWin :: Row -> Row -> Row -> Maybe Marker
tryDiagonalWin (Row a1 b1 c1) (Row a2 b2 c2) (Row a3 b3 c3)
    | a1 == b2 && b2 == c3 = a1 
    | a3 == b2 && b2 == c1 = a3
    | otherwise            = Nothing

checkWinCondition :: StateT Board IO GameStatus
checkWinCondition = StateT $
    \(Board r1 r2 r3) ->
        let horizontal = tryHorizontalWin r1 r2 r3 
            vertical   = tryVerticalWin r1 r2 r3 
            diagonal   = tryDiagonalWin r1 r2 r3
            state      = Board r1 r2 r3
        
        in case (horizontal, vertical, diagonal) of 
            (Just marker, _, _) -> return (Win marker, state) 
            (_, Just marker, _) -> return (Win marker, state)
            (_, _, Just marker) -> return (Win marker, state) 
            _                   -> return (Continue, state)

checkRow :: Row -> Bool 
checkRow (Row a b c) =
    isJust a && isJust b && isJust c

checkStaleMate :: StateT Board IO GameStatus
checkStaleMate = StateT $
    \(Board r1 r2 r3) -> 
        if checkRow r1 && checkRow r2 && checkRow r3 then 
            return (StaleMate, Board r1 r2 r3)
        else 
            return (Continue, Board r1 r2 r3)

parseColumn :: Char -> Validation [MoveError] ColumnLabel 
parseColumn col =
    case toUpper col of 
        'A' -> Success A 
        'B' -> Success B 
        'C' -> Success C
        _   -> Failure [ InvalidColumn col ]

parseRow :: Char -> Validation [MoveError] RowLabel
parseRow row =
    case row of 
        '1' -> Success One 
        '2' -> Success Two 
        '3' -> Success Three
        _   -> Failure [ InvalidRow row ] 

parsePlayerSelection :: String -> Validation [MoveError] (ColumnLabel, RowLabel)
parsePlayerSelection selection =
    case selection of 
        [col,row] -> 
            (,) <$> parseColumn col <*> parseRow row
        _         -> 
            Failure [ InvalidSelection ]

displayMarker :: Maybe Marker -> String 
displayMarker marker =
    case marker of 
        Just mrk -> show mrk 
        Nothing  -> " "

displayRow :: Row -> String
displayRow (Row a b c) = 
    " " ++ 
    displayMarker a ++ " | " ++
    displayMarker b ++ " | " ++
    displayMarker c

displayBoard' :: Board -> [String]
displayBoard' (Board r1 r2 r3) = 
    [ "   A   B   C "
    , "1 " ++ displayRow r1
    , "  -----------"
    , "2 " ++ displayRow r2
    , "  -----------"
    , "3 " ++ displayRow r3
    , "" ]

displayBoard :: Board -> IO ()
displayBoard board = 
    foldMap putStrLn $ displayBoard' board