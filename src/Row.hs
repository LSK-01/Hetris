{-# LANGUAGE RecordWildCards #-}

module Row where

import Data.List
import Data.Maybe
import Data.Vector (Vector, fromList, (!), (//))
import Debug.Trace
import Dimensions
import Graphics.Gloss hiding (Vector)
import Piece

-- Datatypes just to make things clearer and more concise
type Rows = Vector Row

type Row = Vector Block

data Screen = GameOver | Title | Game
  deriving (Eq)

-- Game state definition using record syntax, so we have access to accessor functions
data GameWorld = GameWorld
  { complimentCounter :: Int,
    screen :: Screen,
    scoreFlash :: Int,
    score :: Int,
    currPiece :: Piece,
    rows :: Rows,
    currX :: Float,
    currY :: Float,
    pieceCount :: Int,
    isRotated :: Bool,
    shadowPiece :: Piece
  }

-- Derive 'Eq' so we can compare the value of a Block to one of the possible types
data Block = Empty | Filled
  deriving (Eq)

{- Checks for a filled block directly below the shape. returns True if the row below the shape is not in bounds ie. we are at the bottom of the screen ('not (isInBoundsRow $ rowIndex + 1)'), or returns True if any of the blocks in the row below the shape are filled. We use a list comprehension to move through each block in the row below the piece ('rowIndex + 1'), starting at the leftmost block of the piece ('columnIndex') and iterating through till the end using the generator binded to 'i' -}
--IF HEIGHT IS POSITIVE, WE ADD 1 TO ROWINDEX, OTHERWISE WE NEED TO ADD THE DEPTH OF THE SHAPE
-- WE ALSO NEED TO ADD THE DEPTH TO ROW INDEX WHEN CHECKING FOR THE OUT OF BOUNDS SHIZ
filledBlockBelow :: GameWorld -> Bool
filledBlockBelow world@GameWorld {..} = not (isInBoundsRow $ rowIndex + getDepth currPiece) || or [rows ! (rowIndex + (depths !! i)) ! (columnIndex + i) == Filled | i <- [0 .. getWidth currPiece - 1]]
  where
    rowIndex = coordToRowIndex currY + 1
    columnIndex = coordToColumnIndex currX
    depths = getPieceDepths currPiece

{- Keep moving down the rows, starting from the row below the shape ('rowIndexToCheck', calculated from the y position of the shape passed in), until we reach the bottom of the screen ('isInBoundsRow ...') or until we find a row with
 filled blocks under the current piece. Uses explicit recursion to move down and systematically check each row for filled blocks below the current piece. We call the function
 again with the next row down ('y + blockSize') if we are still in bounds and the current row we are checking has no filled blocks ('null filledBlockIndices'). We use the 'Maybe'
      datatype so that we can return 'Nothing' in the case that there is not a filled block, and then run catMaybes to end up with a list of (just) the indices of the blocks which
       are filled - and then we can check if this list is empty or not to see if there are filled blocks below the shape. -}
--check if the row below the deepest point of the shape is out of bounds first
getClosestFilledCoord :: GameWorld -> Float -> Float
getClosestFilledCoord world@GameWorld {..} y
  | isInBoundsRow rowBelowDeepest && null filledBlockIndices = getClosestFilledCoord world $ y - blockSize
  | isInBoundsRow rowBelowDeepest = rowToCoord (minimum filledBlockIndices) $ getHeight currPiece
  | otherwise = rowToCoord rowIndexToCheck $ getHeight currPiece
  where
    rowBelowDeepest = coordToRowIndex y + getDepth currPiece
    rowIndexToCheck = coordToRowIndex y
    columnIndex = coordToColumnIndex currX
    depths = getPieceDepths currPiece
    filledBlockIndices =
      catMaybes
        [ if rows ! (rowIndexToCheck + (depths !! i)) ! (columnIndex + i) == Filled
            then Just rowIndexToCheck
            else Nothing
          | i <- [0 .. getWidth currPiece - 1]
        ]

{- Creates an updated row, 'updating' here means changing each of the blocks in one of the rows of the GameWorld to now include the current piece the user has set down. A list
 comprehension goes through and changes the blocks to Filled from the start of the shape ('columnIndex') to the end, and then we run an update on the row ('rows ! index') and
  return the value -}
--we check if the input was a depth or a height, and then we use the repesctive profile to see which blocks exactly in that row are filled for the shape. period. this wil l work.
createUpdatedRow :: Int -> GameWorld -> Row
createUpdatedRow height GameWorld {..} = (rows ! rowToUpdate) // [(columnIndex + i, Filled) | i <- if null indices || height == 0 then [0 .. getWidth currPiece - 1] else [head indices .. last indices]]
  where
    columnIndex = coordToColumnIndex currX
    rowIndex = coordToRowIndex currY
    rowToUpdate = rowIndex - height
    absHeight = abs height
    indices = if height <= 0 then elemIndices absHeight $ getPieceDepths currPiece else elemIndices absHeight $ getPieceHeights currPiece

{- Check if every block in the row is filled. numLanesWide is just the width of the window in blockSizes. So we move across the whole row passed in, block by block, and check if
    each one is a filled block, returning True if so and False if not. We then and these values, so that every block has to be filled for isFilled to return true.-}
isFilled :: Row -> Bool
isFilled r = and [r ! (0 + i) == Filled | i <- [0 .. numLanesWide - 1]]

{- Set each row to be equal to the one behind it, and continue to do this while we have filledRows to get rid of - so someone could fill up 3 rows in one go and they all
 disappear. and then just clear out the top row as theres nothing 'behind' the top row. We use explicit recursion to run shiftDownRows again on any remaining filledRows, and pass
  in the remaining filled rows by using filter, with the predicate being all the elements which are not the currFilledRow that we just got rid of. -}
shiftDownRows :: [Int] -> Rows -> Rows
shiftDownRows filledRows rows = if null filledRows then rows // [(0, fromList [Empty | x <- [0 .. numLanesWide - 1]])] else shiftDownRows (filter (/= currFilledRow) filledRows) updatedRows
  where
    currFilledRow = minimum filledRows
    updatedRows = rows // [(i, rows ! (i - 1)) | i <- [1 .. currFilledRow]]

-- Convert our Rows representation of all the blocks to pictures that we can render
rowsToPictures :: Rows -> [Picture]
rowsToPictures rows = catMaybes $ concat [[if rows ! y ! x == Filled then Just (translate (fromIntegral (- gameMaxBoundsWidth + x * round blockSize)) (fromIntegral (gameMaxBoundsHeight - blockSizeInt - (y * round blockSize))) $ color magenta $ polygon [(0, blockSize), (blockSize, blockSize), (blockSize, 0), (0, 0)]) else Nothing | x <- [0 .. numLanesWide - 1]] | y <- [0 .. numLanesHigh - 1]]

initialBlocks :: Rows
initialBlocks = fromList [fromList [Empty | x <- [0 .. numLanesWide - 1]] | y <- [0 .. numLanesHigh - 1]]

getBlockValue :: Rows -> Point -> Block
getBlockValue rows (x, y) = rows ! round y ! round x