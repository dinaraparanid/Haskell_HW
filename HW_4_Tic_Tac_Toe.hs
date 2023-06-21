{-# OPTIONS_GHC -fdefer-typed-holes -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module TicTacToe where

import           CodeWorld
import           Data.List
import           Data.Maybe

-- | Indexes all items in list.
--   Enumeration starts with zero
--
-- >>> enumerate ["A", "B", "C"]
-- [(0,"A"),(1,"B"),(2,"C")]
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

-- | Indexes all cells in the matrix as (row, column).
--   Enumeration starts with zero
--
-- >>> enumerate2D [["A", "B"], ["C", "D"]]
-- [[((0,0),"A"),((0,1),"B")],[((1,0),"C"),((1,1),"D")]]
enumerate2D :: [[a]] -> [[((Int, Int), a)]]
enumerate2D list =
  map (\(i, row) -> map (\(q, e) -> ((i, q), e)) $ enumerate row) $
  enumerate list

-- | Gets element from the list according to the index or
--   produces Nothing if index is not in [0..length]
--
-- >>> getAtOrNothing 1 [1, 2, 3]
-- Just 2
-- >>> getAtOrNothing 3 [1, 2, 3]
-- Nothing
-- >>> getAtOrNothing (-1) [1, 2, 3]
-- Nothing
getAtOrNothing :: Int -> [a] -> Maybe a
getAtOrNothing _ []     = Nothing
getAtOrNothing 0 (x:_)  = Just x
getAtOrNothing i (_:xs) = getAtOrNothing (i - 1) xs

-- | Updates element in the list according to the given index.
--   If index is not in [0..length], data remains unchanged
--
-- >>> updateAt 1 (const 10) [1, 2, 3]
-- [1, 10, 3]
-- >>> updateAt 3 (const 10) [1, 2, 3]
-- [1, 2, 3]
-- >>> updateAt -1 (const 10) [1, 2, 3]
-- [1, 2, 3]
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt _ _ [] = []
updateAt 0 trans (x:xs) = trans x : xs
updateAt i trans (x:xs) = updateInternal (i - 1) trans xs [x]
  where
    updateInternal _ _ [] acc      = acc
    updateInternal 0 tr (n:ns) acc = acc ++ [tr n] ++ ns
    updateInternal q tr (n:ns) acc = updateInternal (q - 1) tr ns (acc ++ [n])

-- | Either empty or marked
type Cell = Maybe Mark

-- | 2D grid of cells
type Board = [[Cell]]

type BoardCoords = (Int, Int)

-- | Mark for tic-tac-toe
data Mark
  = X
  | O
  deriving (Eq, Show)

-- | Tuple with coordinates and a marking symbol
type Streak a = ([BoardCoords], a)

-- | Tuple with coordinates and a mark
type Winner = Streak Mark

-- | Either victory streak or nothing
type WinnerState = Maybe Winner

-- | Game State consists of a winner state and the board itself
data GameState =
  GameState WinnerState Board

-- | Initialises an empty NxM board
initBoard :: Int -> Int -> Board
initBoard n m = replicate n $ replicate m Nothing

-- | Initialises and empty NxM game board
--   and X as the first player
initGameState :: Int -> Int -> GameState
initGameState n m = GameState Nothing $ initBoard n m

-- | Gets next mark after move was done
nextMark ::
     Board -- ˆ Current board
  -> Mark
nextMark brd =
  if crossCnt > zeroCnt
    then O
    else X
  where
    markCnt mrk = length $ filter (== Just mrk) $ concat brd
    crossCnt = markCnt X
    zeroCnt = markCnt O

-- | Gets victory streak from the GameState.
--   If victory was not achieved yet,
--   produces an empty list
victoryCoords :: WinnerState -> [BoardCoords]
victoryCoords winSt =
  case winSt of
    Nothing        -> []
    Just (crds, _) -> crds

-- | Number of cells in streak to open and win
victoryStreakLength :: Int
victoryStreakLength = 3

-- | Run a tic-tac-toe game with an initial game state
ticTacToe :: GameState -> IO ()
ticTacToe gameState =
  activityOf
    gameState
    handleClicks
    (\(GameState winSt board) -> boardPic board $ victoryCoords winSt)

-- | Handles mouse clicks and tries to mark the cell
handleClicks ::
     Event -- ˆ Current event
  -> GameState -- ˆ Current game state
  -> GameState
handleClicks _ (GameState (Just win) brd) = GameState (Just win) brd
handleClicks (PointerPress mouse) (GameState Nothing board) = updatedGS
  where
    updatedGS = do
      let updBoard = putMarkAt (pointToBoardCoords mouse) board
      GameState (winner updBoard) updBoard
handleClicks _ gameState = gameState

-- | Convert mouse position into board coordinates
pointToBoardCoords :: Point -> BoardCoords
pointToBoardCoords (x, y) = (round y, round x)

-- | Attempts to mark the cell if it wasn't already unlocked
markCell ::
     Cell -- ˆ Current cell
  -> Mark -- ˆ Current turn
  -> Cell
markCell c mark =
  case c of
    Nothing -> Just mark
    _       -> c

-- | Attempts to open a cell if it wasn't already unlocked
putMarkAt ::
     BoardCoords -- ˆ Current cell's coordinates
  -> Board -- ˆ Current board
  -> Board
putMarkAt (i, q) brd =
  case getAtOrNothing i brd of
    Nothing -> brd
    Just row -> updateAt i (const updatedRow) brd
      where mark = nextMark brd
            updatedRow = updateAt q (`markCell` mark) row

-- | Tries to determine a winner in a game
winner :: Board -> WinnerState
winner board = getWinner $ filter isLongStreak $ concatMap streaks allLines
  where
    allLines = rows ++ columns ++ diagonals
    rows = enumerate2D board
    columns = transpose rows
    diagonals = leftDiagonals ++ rightDiagonals
    leftDiagonals = leftDiagonalsOf rows
    rightDiagonals = leftDiagonalsOf $ reverse rows
    leftDiagonalsOf b = leftTopDiagonalsOf b ++ leftBottomDiagonalsOf b
    leftTopDiagonalsOf = transpose . zipWith drop [0 ..]
    leftBottomDiagonalsOf = leftTopDiagonalsOf . transpose

-- | Get all consequent streaks ignoring 'Nothing'
streaks ::
     Eq a -- ˆ Mark has to implement Eq to check for marking
  => [(BoardCoords, Maybe a)] -- ˆ Current board state as ((row, col), mark)
  -> [Streak a]
streaks [] = []
streaks ((_, Nothing):xs) = streaks xs
streaks ((curCrd, Just x):xs) = (streakCoords, x) : streaks zs
  where
    (ys, zs) = span (\(_, mrk) -> mrk == Just x) xs
    streakCoords = curCrd : map fst ys

-- | Determine if a streak is long enough to be a winning streak
isLongStreak :: Streak a -> Bool
isLongStreak (streak, _) = length streak >= victoryStreakLength

-- | Tries to get a winning mark
getWinner :: [Streak a] -> Maybe (Streak a)
getWinner = listToMaybe

-- | Draw a single mark (X or O)
markPic :: Mark -> Picture
markPic X =
  scaled 0.4 0.4 $
  rotated (pi / 4) $ solidRectangle 0.5 2 <> solidRectangle 2 0.5
markPic O = thickCircle 0.2 0.3

-- | Draw one board cell at given coordinates
cellAtPic ::
     Int -- ˆ Row coordinate
  -> Int -- ˆ Column coordinate
  -> Cell -- Current cell
  -> Picture
cellAtPic i q cell = translated x y $ rectangle 1 1 <> cellPic
  where
    y = fromIntegral i
    x = fromIntegral q
    cellPic = maybe blank markPic cell

-- | Draw a vicroty (red) board cell at given coordinates
victoryCellAtPic ::
     Int -- ˆ Row coordinate
  -> Int -- ˆ Column coordinate
  -> Cell -- Current cell
  -> Picture
victoryCellAtPic i q cell = colored red $ cellAtPic i q cell

-- | Draw a rectangular board according to the given victory streak
boardPic ::
     Board -- ˆ Current board state
  -> [BoardCoords] -- ˆ Victory streak (or empty list)
  -> Picture
boardPic board winCrds = brdPic
  where
    cellPic i q c =
      if (i, q) `elem` winCrds
        then victoryCellAtPic i q c
        else cellAtPic i q c
    rowPic (i, row) = pictures $ map (uncurry (cellPic i)) $ enumerate row
    brdPic = pictures $ map rowPic $ enumerate board

main :: IO ()
main = ticTacToe $ initGameState 4 5
