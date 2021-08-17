module BoardADT(Board, Cell, Piece, Colour, initboard) where

data Piece =    King
            |   Queen
            |   Rook
            |   Bishop
            |   Knight
            |   Pawn
    deriving (Show, Eq)

data Colour = Black | White
    deriving (Show, Eq)

data Cell = C Piece Colour | Empty
    deriving (Show, Eq)

newtype Board = B [[Cell]]

newtype Coord = (Int, Int)

newtype CoordVec = (Int, Int)

data Move = M Coord Coord
    deriving (show)

initBoard :: Board
initBoard = 
    [
        bm ++ [C Queen Black, C King Black] ++ reverse bm,
        pawns Black,
        emptyR,
        emptyR,
        emptyR,
        emptyR,
        emptyR,
        emptyR,
        pawns White,
        wm ++ [C Queen White, C King White] ++ reverse wm

    ]
        where 
            bm = [C Rook Black, C Bishop Black, C Knight Black]
            wm = [C Rook White, C Bishop White, C Knight White] 
            pawns c = take 8 $ repeat $ C Pawn c
            emptyR = take 8 $ repeat $ Empty


getColour :: Cell -> Maybe Colour
getColour (C _ c) = Just c
getColour _       = Nothing

getMovesForPiece :: Board -> Coord -> [Move]
getMovesForPiece b coor@(f, r)
    | (b !! f) !! r == (C Queen _) = 
        (itermoves b col coor (1, 1) 8) ++
        (itermoves b col coor (-1, 1) 8) ++
        (itermoves b col coor (1, -1) 8) ++
        (itermoves b col coor (-1, -1) 8)
            where col = getColour $ (b !! f) !! r
    | (b !! f) !! r == (Empty) = []

iterMoves :: Board -> Colour -> Coord -> CoordVec -> [Coord]
iterMoves b col coor coorV lim
    | lim > 0 && withinBounds coor = do
        let cell = (b !! (fst coor)) !! snd coor
        case cell of 
            Empty -> coor ++ iterMoves b col (addV coor coorV) (lim - 1)
            getColour cell == col -> []
            _ -> [coor]

addV :: Coord -> CoordVec -> Coord
addV (x, y) (x', y') = (x + x', y + y')

withinBounds :: Coord -> Bool
withinBounds c
    | fst c < 7 && snd c < 7 && fst c >= 0 && snd c >= 0 = True
}   | otherwise = False
