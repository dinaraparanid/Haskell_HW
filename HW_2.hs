{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe

-- Code.World: https://code.world/haskell#PC3nLInLFLdq8NQExapi_4g

-- | -------------------------------- Game Rules --------------------------------
-- In order to open exit door (green one), player has to open all red and blue doors
-- by pressing `Enter` key button on all red/blue buttons. Single button can open
-- only single door. When all red/blue doors are opened, player has to move to the
-- opened green door to finish the game.

-- | -------------------------------- Types and Aliases --------------------------------

type Coords = (Int, Int)
type Row = [Coords]
type Matrix = [Row]

data Tile = Floor | Wall | Exit | ExitOpened | BlueButton | RedButton
  | BlueDoor | RedDoor | BlueDoorOpened | RedDoorOpened | Player

data PlayerMovement = UpMv | DownMv | LeftMv | RightMv

-- Game state with current player's coordinates,
-- last successful movement by player,
-- opened red doors and opened blue doors

data GameState = GameState Coords PlayerMovement (Set.Set Coords) (Set.Set Coords)

-- | -------------------------------- Tiles --------------------------------

gameFieldLen :: Int
gameFieldLen = 20

floorTile :: Picture
floorTile = colored yellow (solidRectangle 0.95 0.95)

wallTile :: Picture
wallTile = colored black (solidRectangle 0.95 0.95)

doorLevel :: Int -> Color -> Picture
doorLevel i c = colored c (solidRectangle (0.19 * (fromIntegral i))  (0.19 * (fromIntegral i)))

doorTileGen :: Int -> Color -> Picture
doorTileGen i c
  | i == 1 = doorLevel 1 black
  | i `mod` 2 == 1 = ((doorTileGen (i - 1) c)) <> (doorLevel i black)
  | otherwise = (doorTileGen (i - 1) c) <> (doorLevel i c)

doorTile :: Color -> Picture
doorTile c = doorTileGen 5 c

openedDoorTile :: Color -> Picture
openedDoorTile c = colored c (solidRectangle 0.95 0.95)

redDoorTile :: Picture
redDoorTile = doorTile red

blueDoorTile :: Picture
blueDoorTile = doorTile blue

exitTile :: Picture
exitTile = doorTile green

openedRedDoorTile :: Picture
openedRedDoorTile = openedDoorTile red

openedBlueDoorTile :: Picture
openedBlueDoorTile = openedDoorTile blue

openedExitTile :: Picture
openedExitTile = openedDoorTile green

buttonTile :: Color -> Picture
buttonTile c = colored c (solidCircle 0.3)

blueButtonTile :: Picture
blueButtonTile = (buttonTile blue) <> floorTile

redButtonTile :: Picture
redButtonTile = (buttonTile red) <> floorTile

-- Rotates/reflects 🚶 according to the given movement

playerTile :: PlayerMovement -> Picture
playerTile UpMv = rotated (-pi / 2) (lettering "🚶")
playerTile DownMv = reflected (pi / 2) (rotated (pi / 2) (lettering "🚶"))
playerTile LeftMv = lettering "🚶"
playerTile RightMv = reflected (pi / 2) (lettering "🚶")

-- Translates tile to the given position in integers

translatedTile :: Coords -> Picture -> Picture
translatedTile (f, s) tile = translated (fromIntegral f) (fromIntegral s) tile

-- Produces image of the given tile by its coordinates and player's position

tilePic :: (Coords, [Tile], PlayerMovement) -> Picture
tilePic (c, [Wall], _) = translatedTile c wallTile
tilePic (c, [Exit], _) = translatedTile c exitTile
tilePic (c, [Floor], _) = translatedTile c floorTile
tilePic (c, [RedDoor], _) = translatedTile c redDoorTile
tilePic (c, [BlueDoor], _) = translatedTile c blueDoorTile
tilePic (c, [RedButton], _) = translatedTile c redButtonTile
tilePic (c, [BlueButton], _) = translatedTile c blueButtonTile
tilePic (c, [ExitOpened], _) = translatedTile c openedExitTile
tilePic (c, [RedDoorOpened], _) = translatedTile c openedRedDoorTile
tilePic (c, [BlueDoorOpened], _) = translatedTile c openedBlueDoorTile
tilePic (c, [Player, Exit], mv) = translatedTile c ((playerTile mv) <> exitTile)
tilePic (c, [Player, Floor], mv) = translatedTile c ((playerTile mv) <> floorTile)
tilePic (c, [Player, RedDoor], mv) = translatedTile c ((playerTile mv) <> redDoorTile)
tilePic (c, [Player, BlueDoor], mv) = translatedTile c ((playerTile mv) <> blueDoorTile)
tilePic (c, [Player, RedButton], mv) = translatedTile c ((playerTile mv) <> redButtonTile)
tilePic (c, [Player, BlueButton], mv) = translatedTile c ((playerTile mv) <> blueButtonTile)
tilePic (c, [Player, ExitOpened], mv) = translatedTile c ((playerTile mv) <> openedExitTile)
tilePic (c, [Player, RedDoorOpened], mv) = translatedTile c ((playerTile mv) <> openedRedDoorTile)
tilePic (c, [Player, BlueDoorOpened], mv) = translatedTile c ((playerTile mv) <> openedBlueDoorTile)

-- | -------------------------------- Position Handling --------------------------------

-- Checks if coordinates belong to the floor's set of coordinates.
-- Implemented with an explicit check row by row

isFloor :: Coords -> Bool
isFloor (f, s)
  | s == 1 && f >= 1 && f <= 3 = True
  | s == 1 && f >= 5 && f <= 13 = True
  | s == 1 && f >= 15 && f <= 17 = True
  | s == 1 && f == 19 = True
  | s == 2 && f == 3 = True
  | s == 2 && f == 5 = True
  | s == 2 && f == 7 = True
  | s == 2 && f == 10 = True
  | s == 2 && f == 17 = True
  | s == 2 && f == 19 = True
  | s == 3 && f >= 1 && f <= 3 = True
  | s == 3 && f == 5 = True
  | s == 3 && f == 7 = True
  | s == 3 && f >= 10 && f <= 17 = True
  | s == 3 && f == 19 = True
  | s == 4 && f == 1 = True
  | s == 4 && f == 5 = True
  | s == 4 && f == 7 = True
  | s == 4 && f == 19 = True
  | s == 5 && f >= 1 && f <= 5 = True
  | s == 5 && f >= 7 && f <= 19 = True
  | s == 6 && f == 13 = True
  | s == 6 && f == 15 = True
  | s == 6 && f == 17 = True
  | s == 7 && f >= 1 && f <= 11 = True
  | s == 7 && f == 13 = True
  | s == 7 && f == 15 = True
  | s == 7 && f == 17 = True
  | s == 7 && f == 19 = True
  | s == 8 && f == 1 = True
  | s == 8 && f == 11 = True
  | s == 8 && f == 15 = True
  | s == 8 && f == 17 = True
  | s == 8 && f == 19 = True
  | s == 9 && f >= 1 && f <= 3 = True
  | s == 9 && f >= 5 && f <= 13 = True
  | s == 9 && f == 15 = True
  | s == 9 && f == 17 = True
  | s == 9 && f == 19 = True
  | s == 10 && f == 1 = True
  | s == 10 && f == 3 = True
  | s == 10 && f == 13 = True
  | s == 10 && f == 15 = True
  | s == 10 && f == 17 = True
  | s == 10 && f == 19 = True
  | s == 11 && f == 1 = True
  | s == 11 && f == 3 = True
  | s == 11 && f >= 5 && f <= 9 = True
  | s == 11 && f == 11 = True
  | s == 11 && f >= 13 && f <= 15 = True
  | s == 11 && f >= 17 && f <= 19 = True
  | s == 12 && f == 3 = True
  | s == 12 && f == 5 = True
  | s == 12 && f == 9 = True
  | s == 12 && f == 11 = True
  | s == 12 && f == 15 = True
  | s == 12 && f == 19 = True
  | s == 13 && f >= 1 && f <= 3 = True
  | s == 13 && f == 5 = True
  | s == 13 && f >= 7 && f <= 9 = True
  | s == 13 && f >= 11 && f <= 15 = True
  | s == 13 && f == 17 = True
  | s == 13 && f == 19 = True
  | s == 14 && f == 1 = True
  | s == 14 && f == 5 = True
  | s == 14 && f == 7 = True
  | s == 14 && f == 11 = True
  | s == 14 && f == 17 = True
  | s == 14 && f == 19 = True
  | s == 15 && f >= 1 && f <= 5 = True
  | s == 15 && f == 7 = True
  | s == 15 && f == 9 = True
  | s == 15 && f == 11 = True
  | s == 15 && f >= 13 && f <= 17 = True
  | s == 15 && f == 19 = True
  | s == 16 && f == 1 = True
  | s == 16 && f == 7 = True
  | s == 16 && f == 9 = True
  | s == 16 && f == 11 = True
  | s == 16 && f == 15 = True
  | s == 16 && f == 17 = True
  | s == 16 && f == 19 = True
  | s == 17 && f == 1 = True
  | s == 17 && f >= 3 && f <= 5 = True
  | s == 17 && f == 7 = True
  | s == 17 && f >= 9 && f <= 15 = True
  | s == 17 && f == 17 = True
  | s == 17 && f == 19 = True
  | s == 18 && f == 1 = True
  | s == 18 && f == 5 = True
  | s == 18 && f == 19 = True
  | s == 19 && f >= 1 && f <= 5 = True
  | s == 19 && f >= 7 && f <= 19 = True
  | otherwise = False

blueButtonsCoords :: [Coords]
blueButtonsCoords = [(13, 1), (19, 7), (7, 17), (13, 15)]

isBlueButtonCoord :: Coords -> Bool
isBlueButtonCoord coords = elem coords blueButtonsCoords

blueDoorsCoords :: [Coords]
blueDoorsCoords = [(15, 1), (19, 1), (3, 17), (9, 15)]

redButtonsCoords :: [Coords]
redButtonsCoords = [(17, 17), (13, 7), (1, 11)]

isRedButtonCoord :: Coords -> Bool
isRedButtonCoord coords = elem coords redButtonsCoords

redDoorsCoords :: [Coords]
redDoorsCoords = [(17, 13), (11, 11), (5, 9)]

-- Opens door by given button's index.
-- Implementation relates on tree-sets to store only unique values.
-- 'index' must be valid number in 0 .. (length doorsCoords).
-- 'doorsCoords' are either blue or red doors' coordinates.
-- 'openedDoorsCoords' are either opened blue or red doors' coordinates

openDoor :: Int -> [Coords] -> Set.Set Coords -> Set.Set Coords
openDoor index doorsCoords openedDoorsCoords = Set.insert (doorsCoords !! index) openedDoorsCoords

-- Opens blue door by given button's index.
-- Implementation relates on tree-sets to store only unique values.
-- 'index' must be valid number in 0 .. (length blueDoorsCoords).
-- 'openedDoorsCoords' are currently opened blue doors' coordinates

openBlueDoor :: Int -> Set.Set Coords -> Set.Set Coords
openBlueDoor index openedDoorsCoords = openDoor index blueDoorsCoords openedDoorsCoords

-- Opens red door by given button's index.
-- Implementation relates on tree-sets to store only unique values.
-- 'index' must be valid number in 0 .. (length redDoorsCoords).
-- 'openedDoorsCoords' are currently opened red doors' coordinates

openRedDoor :: Int -> Set.Set Coords -> Set.Set Coords
openRedDoor index openedDoorsCoords = openDoor index redDoorsCoords openedDoorsCoords

-- Checks if all red and blue doors are opened

isExitOpened :: Set.Set Coords -> Set.Set Coords -> Bool
isExitOpened redDoorsOpened blueDoorsOpened = length redDoorsOpened + length blueDoorsOpened == 7

-- Checks if all red and blue doors are opened and player has reached the door

isGameFinished :: Coords -> Set.Set Coords -> Set.Set Coords -> Bool
isGameFinished (f, s) redDoorsOpened blueDoorsOpened = f == 7 && s == 19 && isExitOpened redDoorsOpened blueDoorsOpened

-- Produces map's state for the current cell in the map by given coordinates and the game state.
-- Returns coordinates for presentation, all tiles on the given position and last player's movement

myLevelMap :: Coords -> GameState -> (Coords, [Tile], PlayerMovement)
myLevelMap (f, s) (GameState (playerF, playerS) mv redDoorsOpened blueDoorsOpened)
  | f == playerF && s == playerS && elem (f, s) blueButtonsCoords = ((f, -s), [Player, BlueButton], mv)
  | f == playerF && s == playerS && elem (f, s) redButtonsCoords = ((f, -s), [Player, RedButton], mv)
  | f == playerF && s == playerS && Set.member (f, s) blueDoorsOpened = ((f, -s), [Player, BlueDoorOpened], mv)
  | f == playerF && s == playerS && Set.member (f, s) redDoorsOpened = ((f, -s), [Player, RedDoorOpened], mv)
  | f == playerF && s == playerS && elem (f, s) blueDoorsCoords = ((f, -s), [Player, BlueDoor], mv)
  | f == playerF && s == playerS && elem (f, s) redDoorsCoords = ((f, -s), [Player, RedDoor], mv)
  | f == playerF && s == playerS && f == 7 && s == 19 && (isExitOpened redDoorsOpened blueDoorsOpened) = ((f, -s), [Player, ExitOpened], mv)
  | f == playerF && s == playerS && f == 7 && s == 19 = ((f, -s), [Player, Exit], mv)
  | f == playerF && s == playerS && isFloor (f, s) = ((f, -s), [Player, Floor], mv)
  | elem (f, s) blueButtonsCoords = ((f, -s), [BlueButton], mv)
  | elem (f, s) redButtonsCoords = ((f, -s), [RedButton], mv)
  | Set.member (f, s) blueDoorsOpened = ((f, -s), [BlueDoorOpened], mv)
  | Set.member (f, s) redDoorsOpened = ((f, -s), [RedDoorOpened], mv)
  | elem (f, s) blueDoorsCoords = ((f, -s), [BlueDoor], mv)
  | elem (f, s) redDoorsCoords = ((f, -s), [RedDoor], mv)
  | f == 7 && s == 19 && (isExitOpened redDoorsOpened blueDoorsOpened) = ((f, -s), [ExitOpened], mv)
  | f == 7 && s == 19 = ((f, -s), [Exit], mv)
  | isFloor (f, s) = ((f, -s), [Floor], mv)
  | otherwise = ((f, -s), [Wall], mv)

-- | -------------------------------- Map Generation --------------------------------

row :: Int -> Row
row ind = map (\i -> (ind, (gameFieldLen - i))) [0..gameFieldLen]

matrix :: Matrix
matrix = map(\i -> row (gameFieldLen - i)) [0..gameFieldLen]

rowPicture :: Row -> GameState -> Picture
rowPicture row gameState = foldl1 (<>) (map (tilePic) (map (\c -> myLevelMap c gameState) row))

matrixPicture :: Matrix -> GameState -> Picture
matrixPicture mtx gameState = foldl1 (<>) (map (\r -> rowPicture r gameState) mtx)

myLevelMapPicture :: GameState -> Picture
myLevelMapPicture (GameState (playerF, playerS) mv redDoorsOpened blueDoorsOpened) =
  if isGameFinished (playerF, playerS) redDoorsOpened blueDoorsOpened then lettering "Victory!"
  else matrixPicture  matrix (GameState (playerF, playerS) mv redDoorsOpened blueDoorsOpened)

main :: IO ()
main = debugActivityOf initialGameState updatePlayerCoordinate gameState
  where
    initialGameState = GameState (1, 1) RightMv Set.empty Set.empty

    gameState gameState = myLevelMapPicture gameState

    updatePlayerCoordinate event (GameState (playerF, playerS) mv redDoorsOpened blueDoorsOpened) =
      case event of
        KeyPress "Up" -> if not (isGameFinished (playerF, playerS) redDoorsOpened blueDoorsOpened) && isFloor (playerF, (playerS - 1)) then
          (GameState (playerF, (playerS - 1)) UpMv redDoorsOpened blueDoorsOpened)
          else (GameState (playerF, playerS) mv redDoorsOpened blueDoorsOpened)

        KeyPress "Down" -> if not (isGameFinished (playerF, playerS) redDoorsOpened blueDoorsOpened) && isFloor (playerF, (playerS + 1)) then
          (GameState (playerF, (playerS + 1)) DownMv redDoorsOpened blueDoorsOpened)
          else (GameState (playerF, playerS) mv redDoorsOpened blueDoorsOpened)

        KeyPress "Left" -> if not (isGameFinished (playerF, playerS) redDoorsOpened blueDoorsOpened) && isFloor ((playerF - 1), playerS) then
          (GameState ((playerF - 1), playerS) LeftMv redDoorsOpened blueDoorsOpened)
          else (GameState (playerF, playerS) mv redDoorsOpened blueDoorsOpened)

        KeyPress "Right" -> if not (isGameFinished (playerF, playerS) redDoorsOpened blueDoorsOpened) && isFloor ((playerF + 1), playerS) then
          (GameState ((playerF + 1), playerS) RightMv redDoorsOpened blueDoorsOpened)
          else (GameState (playerF, playerS) mv redDoorsOpened blueDoorsOpened)

        KeyPress "Enter" -> if not (isGameFinished (playerF, playerS) redDoorsOpened blueDoorsOpened) && isBlueButtonCoord (playerF, playerS) then
          (GameState (playerF, playerS) mv redDoorsOpened (openBlueDoor (Maybe.fromJust (List.elemIndex (playerF, playerS) blueButtonsCoords)) blueDoorsOpened))
          else if not (isGameFinished (playerF, playerS) redDoorsOpened blueDoorsOpened) && isRedButtonCoord (playerF, playerS) then
            (GameState (playerF, playerS) mv (openRedDoor (Maybe.fromJust (List.elemIndex (playerF, playerS) redButtonsCoords)) redDoorsOpened) blueDoorsOpened)
          else (GameState (playerF, playerS) mv redDoorsOpened blueDoorsOpened)  

        _ -> (GameState (playerF, playerS) mv redDoorsOpened blueDoorsOpened)
