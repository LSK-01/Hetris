module Dimensions where

import Data.Vector
import Graphics.Gloss (Point)

-- Particularly proud of the fact that one can simply change gameHeight, gameWidth, and blockSize, to any values they want, and everything will work on the new dimensions specified. Go ahead and try it!
-- (NOTE -- gameHeight, gameWidth and blockSize have to all be divisible by each other if you catch my drift but otherwise it will work)
gameHeight :: Int
gameHeight = 400

gameWidth :: Int
gameWidth = 200

blockSize :: Float
blockSize = 20

initialCurrY :: Float
initialCurrY = fromIntegral gameMaxBoundsHeight - blockSize

initialCurrX :: Float 
initialCurrX = fromIntegral $ -gameMaxBoundsWidth + blockSizeInt

gameMaxBoundsHeight :: Int
gameMaxBoundsHeight = gameHeight `div` 2

gameMaxBoundsWidth :: Int
gameMaxBoundsWidth = gameWidth `div` 2

blockSizeInt :: Int
blockSizeInt = round blockSize

-- Calculate the number of 'lanes' ie columns and rows of blocks there are in the game
numLanesWide :: Int
numLanesWide = gameWidth `div` blockSizeInt

numLanesHigh :: Int
numLanesHigh = gameHeight `div` blockSizeInt

-- Boring maths which took me ages to figure out
coordToColumnIndex :: Float -> Int
coordToColumnIndex x = abs ((round x + gameMaxBoundsWidth) `div` blockSizeInt)

coordToRowIndex :: Float -> Int
coordToRowIndex y = abs ((round y - gameMaxBoundsHeight) `div` blockSizeInt) - 1

isInBoundsRow :: Int -> Bool
isInBoundsRow i = i < gameHeight `div` blockSizeInt && i >= 0

rowToCoord :: Int -> Int -> Float
rowToCoord i h = fromIntegral $ (gameMaxBoundsHeight - blockSizeInt) - (i * blockSizeInt) + 1 * blockSizeInt