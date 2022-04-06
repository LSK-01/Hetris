module Piece where

import Dimensions
import Graphics.Gloss

-- Derive enum so we can get the 'next' piece to be spawned in by using 'succ'
-- New pieces can be added easily by adding to the Piece datatype, and the 3 piece functions
--get rid of some of the wank pieces, look up wath the actual tetris pieces are. 
data Piece = Square | HLine | TBar | TBar90 | TBar270 | TBar180 | HLine90
  deriving (Enum, Eq, Ord) 

createPolygon :: Float -> Color -> (Int, Int) -> (Float, Float) -> Picture
createPolygon alpha c (w, h) (x, y) = color (withAlpha alpha c) $ polygon [(0 + xOf, 0 + yOf), (blockSize * wF + xOf, 0 + yOf), (blockSize * wF + xOf, blockSize * hF + yOf), (0 + xOf, blockSize * hF + yOf)]
  where
    wF = fromIntegral w
    hF = fromIntegral h
    xOf = x * blockSize 
    yOf = y * blockSize

-- Convert a type of Piece to a coloured and drawn Polygon picture
pieceToPicture :: Piece -> Float -> Picture
pieceToPicture p alpha = case p of
  Square -> createPolygon alpha yellow (getPieceDimensions Square) (0, 0)
  HLine -> createPolygon alpha cyan (getPieceDimensions HLine) (0, 0)
  HLine90 -> createPolygon alpha cyan (1, 4) (0, 0)
  TBar -> pictures [createPolygon alpha violet (3, 1) (0, 0), createPolygon alpha violet (1, 1) (1, -1)]
  TBar90 -> pictures [createPolygon alpha violet (1, 3) (0, -1), createPolygon alpha violet (1, 1) (1, 0)]
  TBar180 -> pictures [createPolygon alpha violet (3, 1) (0, 0), createPolygon alpha violet (1, 1) (1, 1)]
  TBar270 -> pictures [createPolygon alpha violet (1, 3) (1, -1), createPolygon alpha violet (1, 1) (0, 0)]



-- Define dimensions for each piece
--Width is the first value of the tuple, and is the width of the shape at the height/depth 0, and should also be the widest part of the shape
-- Height is the second value
--just get rido f this and calculate everything from list lengths cus it wont be inefficient cus theyre short lists
getPieceDimensions :: Piece -> (Int, Int)
getPieceDimensions p = case p of
  TBar90 -> (2, 2)
  TBar -> (3, 1)
  TBar180 -> (3, 2)
  TBar270 -> (2, 2)
  Square -> (2, 2)
  HLine -> (4, 1)
  HLine90 -> (1, 4)

-- Define depth at each column for each shape. (how far it extends below the (0,0) leftest bottom block of the shape)
getPieceDepths :: Piece -> [Int]
getPieceDepths p = case p of
  TBar -> [0, 1, 0]
  TBar270 -> [0, 1]
  TBar90 -> [1, 0]
  -- all blocky shapes have no depth
  _ -> replicate (fst $ getPieceDimensions p) 0

-- block heights for each column of the shape
getPieceHeights :: Piece -> [Int]
getPieceHeights p = case p of
  Square -> [1, 1]
  TBar90 -> [1,0]
  TBar270 -> [0, 1]
  TBar180 -> [0, 1, 0]
  HLine90 -> [3]
  _ -> [snd (getPieceDimensions p) - 1 | x <- [1..fst $ getPieceDimensions p]]

rotatePiece :: Piece -> Piece
rotatePiece p = case p of
  TBar -> TBar90
  TBar90 -> TBar180
  TBar180 -> TBar270
  TBar270 -> TBar
  HLine -> HLine90
  HLine90 -> HLine
  _ -> p

-- We have to declare our own wrapper function for 'succ' so that we can specify a wrap around for when we get to the final value - otherwise 'succ' just fails
nextPiece :: Piece -> Piece
nextPiece p
  | p == HLine90 = TBar
  | p >= TBar = Square
  | otherwise =  succ p

getHeight :: Piece -> Int
getHeight = snd . getPieceDimensions

getWidth :: Piece -> Int
getWidth = fst . getPieceDimensions

getDepth :: Piece -> Int
getDepth = maximum . getPieceDepths