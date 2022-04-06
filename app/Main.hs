{- Using RecordWildCards language extension lets us pattern match on our GameWorld constructor and automatically creates bindings for all fields of GameWorld, so we dont have to
 run our accessor on the argument each time. the '@' syntax eg 'world@GameWorld {..}' just means we can use world to reference the whole GameWorld datatype, as if we had still
    just passed it in as an argument without pattern matching.-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Data.Vector hiding (and, catMaybes, concat, filter, head, last, length, maximum, minimum, null, or, (++))
import qualified Data.Vector as V (length)
import Dimensions
import qualified Graphics.Gloss as G (text)
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game hiding (Vector)
import Numeric
import Piece
import System.Random
import Prelude hiding (length)
import Row
import Debug.Trace

-- A list of nice compliments :)
compliments :: Vector [Char]
compliments = fromList ["Amazing m8!", "Noice one!", "Steady on!", "Another one?!?", "You crazy", "Take it easy!", "Cheater?!", "Mmhm honey!"]

complimentsLength :: Int
complimentsLength = V.length compliments

-- main function to interact with the entire real world
main :: IO ()
main =
  play
    windowDisplay
    white -- background color
    simulationRate -- 'fps'
    newGame -- initial game state
    renderer -- render polygons
    inputHandler
    updator -- runs every frame

windowDisplay :: Display
windowDisplay = InWindow "Hetris" (gameWidth, gameHeight) (10, 10)

newGame :: GameWorld
newGame =
  GameWorld
    { complimentCounter = 0,
      screen = Title,
      scoreFlash = 0,
      score = 0,
      currPiece = Square,
      rows = initialBlocks,
      currX = initialCurrX,
      currY = initialCurrY,
      pieceCount = 0,
      isRotated = False,
      shadowPiece = Square
    }

simulationRate :: Int
simulationRate = 5

-- Helper function to simplify turning text given a specific size and position into a picture
textToPic :: String -> Float -> (Float, Float) -> Picture
textToPic s y scales = color magenta $ translate (fromIntegral (- gameMaxBoundsWidth)) (fromIntegral gameMaxBoundsHeight - y * blockSize) $ uncurry scale scales $ text s

-- Renders all our pictures into the game
renderer :: GameWorld -> Picture
renderer world@GameWorld {..} =
  case screen of
    -- Render the Rows representation of our blocks, the current polygon, and our 'score flash' which we append if 'scoreFlash' has a value
    Game -> pictures $ if scoreFlash /= 0 then textToPic (compliments ! (complimentCounter `mod` complimentsLength) ++ " +" ++ show scoreFlash) 1 (0.15, 0.15) : gamePictures else gamePictures
    Title -> pictures titleTextPics
    GameOver -> pictures $ textToPic ("Final score " ++ show score) 1 (0.1, 0.1) : gameOverPics
  where
    gamePictures = [currPolyPicture, shadowPicture] ++ rowsToPictures rows
    shadowPicture = translate currX (getClosestFilledCoord world $ currY - blockSize) (pieceToPicture currPiece 0.5)
    currPolyPicture = translate currX currY $ pieceToPicture currPiece 1
    gameOverPics = [textToPic "Game Over!" 4 (0.1, 0.1), textToPic "SPACE to play again" 7 (0.1, 0.1)]
    titleTextPics = [textToPic "Welcome to Hetris!" 1 (0.1, 0.1), textToPic "Press SPACE to start." 2 (0.1, 0.1), textToPic "Use the arrow keys to move a block" 6 (0.1, 0.1), textToPic "Use SPACE to rotate a piece" 4 (0.1, 0.1)]

-- Checks for whenever a user presses a key
inputHandler :: Event -> GameWorld -> GameWorld
-- "Nested" pattern matching here - pattern match on the key if it is a SpecialKey, and then pattern match again using a case of ... statement
inputHandler (EventKey (SpecialKey k) Down _ _) world@GameWorld {..} =
  -- Change the current screen to be playing the game, on a freshly initialised world, if any SpecialKey is pressed
  -- Else pattern match and do something accordingly
  if screen /= Game then newGame {screen = Game} else
  -- block to the right of the piece, taking into account the piece width. We use this so the user can't just move into other blocks to the side of the piece they are playing with
  let blockToRight = (rows ! coordToRowIndex currY) ! (coordToColumnIndex currX + getWidth currPiece)
      blockToLeft = (rows ! coordToRowIndex currY) ! (coordToColumnIndex currX - 1) -- As above but for the left side of the piece
   in case k of
        -- If there is space to move to the right and the block is empty
        KeyRight -> if round currX < (gameMaxBoundsWidth - (getWidth currPiece * blockSizeInt)) && blockToRight == Empty then world {currX = currX + blockSize} else world
        -- As above
        KeyLeft -> if round currX > - gameMaxBoundsWidth && blockToLeft == Empty then world {currX = currX - blockSize} else world
        -- Find the closest row with a filled block below the width profile of the shape, and move the shape's current Y position to be at this row
        KeyDown -> if downKeyPos < currY then (world {currY = downKeyPos}) else world
        --Only allow rotation if there is 'space' to do so, ie rotating would not move the new shape out of bounds
        KeySpace -> if currX + fromIntegral (getHeight currPiece) * blockSize < fromIntegral gameMaxBoundsWidth then world {currPiece = rotatePiece currPiece} else world
        -- If any other keys are pressed, just return back the game world unchanged
        _ -> world
        where
          downKeyPos = getClosestFilledCoord world $ currY - blockSize 
inputHandler _ world = world


{- Brings in the next polygon to be handled by the user, sets the last one into 'rows', and if the user managed to fill a row we do the shifting down and show a score in scoreFlash. We also check if the user has filled up the screen with blocks, and if so move to the GameOver screen. -}
bringNextPoly :: GameWorld -> GameWorld
bringNextPoly world@GameWorld {..} =
  if isInBoundsRow $ rowIndex - getHeight currPiece then newWorld else world {screen = GameOver}
  where
    columnIndex = coordToColumnIndex currX
    rowIndex = coordToRowIndex currY
    -- updatedRows is just the result of calling createUpdatedRow for the whole height of the piece, so no matter the dimension of the piece we fill in all appropriate blocks in 'rows' as required
    updatedRows = rows // [(rowIndex - k, createUpdatedRow k world) | k <- [-(getDepth currPiece) .. getHeight currPiece - 1]]
    -- We then take the updatedRows and check if the user managed to fill any rows completely. We use 'Nothing' to represent a row index that wasn't filled, and then run catMaybes to get a list of all the row indices which were filled by the user using this shape. We only need to check the rows that the shape is occupying and use a list comprehension to do this.
    filledRows = catMaybes [if isFilled (updatedRows ! (rowIndex - k)) then Just (rowIndex - k) else Nothing | k <- [-(getDepth currPiece) .. getHeight currPiece - 1]]
    -- To be used in scoreFlash and to update the score we show at the GameOver screen
    scoreNum = length filledRows
    newWorld =
      world
        { complimentCounter = if scoreNum /= 0 then complimentCounter + 1 else complimentCounter,
          score = score + scoreNum,
          scoreFlash = scoreNum,
          -- If we had no filled rows then just return back updatedRows otherwise do any required shifting to get rid of filled rows, done by shiftDownRows
          rows = if null filledRows then updatedRows else shiftDownRows filledRows updatedRows,
          currPiece = nextPiece currPiece,
          currX = initialCurrX,
          currY = initialCurrY,
          pieceCount = pieceCount + 1,
          isRotated = False,
          shadowPiece = nextPiece currPiece
        }

{- Mispelled updater because it looks cooler. 'updator' just does a couple things. We either call bringNextPoly (if the shape has reached the bottom of the screen or a filled block), or we just shift the shape down by a blockSize. -}
updator :: Float -> GameWorld -> GameWorld
updator dt world@GameWorld {..} = if currY == fromIntegral (-gameMaxBoundsHeight) || filledBlockBelow world then bringNextPoly world else world {currY = gravity currY}
  where
    gravity :: Float -> Float
    gravity c = if screen == Game then c - blockSize else c
