module Types where

newtype Player = Player String deriving (Eq, Show)

data Marker = X | O deriving (Eq, Show)

data RowLabel = One | Two | Three deriving Show

data ColumnLabel = A | B | C deriving Show

data Row = Row (Maybe Marker) (Maybe Marker) (Maybe Marker) deriving (Eq, Show)

data Board = Board Row Row Row deriving (Eq, Show)

data MoveError = PositionUnavailable
               | InvalidColumn Char
               | InvalidRow Char
               | InvalidSelection
               deriving Show

data GameStatus = Win Marker 
                | StaleMate 
                | Continue 
                | End
                | Error [MoveError] deriving Show