{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module EscapeTheRoom where

import           CodeWorld
import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Set   as Set
import qualified Data.Text  as Text

-- | -------------------------------- Game Rules --------------------------------
--
-- In order to open exit door (green one), player has to open all red and blue doors
-- by pressing `Enter` key button on all red/blue buttons. Single button can open
-- only single door. When all red/blue doors are opened, player has to move to the
-- opened green door to finish the game.
--
-- | -------------------------------- Types and Aliases --------------------------------
--
type Coords = (Int, Int)

type Row = [Coords]

type Matrix = [Row]

-- | The type of an 'activityOf' and 'debugActivityOf' functions
type ActivityOf world
   = world -> (Event -> world -> world) -> (world -> Picture) -> IO ()

-- | Interaction state for 'world' with start screen
data WithStartScreen world
  = StartScreen -- ˆ Start screen
  | GameOn world -- ˆ Game is on with 'world' state

data Tile
  = Floor
  | Wall
  | Exit
  | ExitOpened
  | BlueButton
  | RedButton
  | BlueDoor
  | RedDoor
  | BlueDoorOpened
  | RedDoorOpened
  | Player

data PlayerMovement
  = UpMv
  | DownMv
  | LeftMv
  | RightMv

-- | Game state with current player's coordinates,
--   last successful movement by player,
--   opened red doors and opened blue doors
data GameState =
  GameState Coords PlayerMovement (Set.Set Coords) (Set.Set Coords)

-- | Enum for 'withPaddingFromCenter'
--   to layout items in desirable order
--   (either Row or Column layouts)
data ContainerView
  = Row
  | Column

-- | Wrapper over zip to index all items in list.
--   Enumeration starts with zero
--
-- >>> enumerate ["A", "B", "C"]
-- [(0,"A"),(1,"B"),(2,"C")]
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

-- | Places all pictures with the given container's layout and padding.
--   Note that CodeWorld does not provide 'constraints' functionality,
--   that is why padding is calculated from the center for each view
withPaddingFromCenter ::
     ContainerView -- ˆ Row or Column
  -> Double -- ˆ Content padding
  -> [Picture] -- ˆ Pictures to place
  -> Picture
withPaddingFromCenter Row padding items =
  foldl
    (<>)
    blank
    (map
       (\(i, pic) -> translated (fromIntegral i * padding) 0 pic)
       (enumerate items))
withPaddingFromCenter Column padding items =
  foldl
    (<>)
    blank
    (map
       (\(i, pic) -> translated 0 (fromIntegral i * padding) pic)
       (enumerate items))

-- | Transforms all items to Pictures and places them
--   with the given container's layout and padding.
--   Note that CodeWorld does not provide 'constraints' functionality,
--   that is why padding is calculated from the center for each view
mapWithPaddingFromCenter ::
     ContainerView -- ˆ Row or Column
  -> Double -- ˆ Content padding
  -> (a -> Picture) -- ˆ Item to picture transformation
  -> [a] -- ˆ Pictures to place
  -> Picture
mapWithPaddingFromCenter Row padding trans items =
  foldl
    (<>)
    blank
    (map
       (\(i, x) -> translated (fromIntegral i * padding) 0 $ trans x)
       (enumerate items))
mapWithPaddingFromCenter Column padding trans items =
  foldl
    (<>)
    blank
    (map (\(i, x) -> translated 0 (fromIntegral i * padding) $ trans x) $
     enumerate items)

-- | -------------------------------- Tiles --------------------------------
gameFieldLen :: Int
gameFieldLen = 20

floorTile :: Picture
floorTile = colored yellow $ solidRectangle 0.95 0.95

wallTile :: Picture
wallTile = colored black $ solidRectangle 0.95 0.95

doorLevel :: Int -> Color -> Picture
doorLevel i c =
  colored c $ solidRectangle (0.19 * fromIntegral i) (0.19 * fromIntegral i)

doorTileGen :: Int -> Color -> Picture
doorTileGen i c
  | i == 1 = doorLevel 1 black
  | i `mod` 2 == 1 = doorTileGen (i - 1) c <> doorLevel i black
  | otherwise = doorTileGen (i - 1) c <> doorLevel i c

doorTile :: Color -> Picture
doorTile = doorTileGen 5

openedDoorTile :: Color -> Picture
openedDoorTile c = colored c $ solidRectangle 0.95 0.95

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
buttonTile c = colored c $ solidCircle 0.3

blueButtonTile :: Picture
blueButtonTile = buttonTile blue <> floorTile

redButtonTile :: Picture
redButtonTile = buttonTile red <> floorTile

-- | Rotates/reflects 🚶 according to the given movement
playerTile :: PlayerMovement -> Picture
playerTile UpMv    = rotated (-pi / 2) $ lettering "🚶"
playerTile DownMv  = reflected (pi / 2) $ rotated (pi / 2) $ lettering "🚶"
playerTile LeftMv  = lettering "🚶"
playerTile RightMv = reflected (pi / 2) $ lettering "🚶"

-- | Translates tile to the given position in integers
translatedTile :: Coords -> Picture -> Picture
translatedTile (f, s) = translated (fromIntegral f) (fromIntegral s)

-- | Produces image of the given tile by its coordinates and player's position
tilePic :: (Coords, [Tile], PlayerMovement) -> Picture
tilePic (c, arr, mv) =
  translatedTile
    c
    (case arr of
       [Wall]                   -> wallTile
       [Exit]                   -> exitTile
       [Floor]                  -> floorTile
       [RedDoor]                -> redDoorTile
       [BlueDoor]               -> blueDoorTile
       [RedButton]              -> redButtonTile
       [BlueButton]             -> blueButtonTile
       [ExitOpened]             -> openedExitTile
       [RedDoorOpened]          -> openedRedDoorTile
       [BlueDoorOpened]         -> openedBlueDoorTile
       [Player, Exit]           -> playerTranslatedTile <> exitTile
       [Player, Floor]          -> playerTranslatedTile <> floorTile
       [Player, RedDoor]        -> playerTranslatedTile <> redDoorTile
       [Player, BlueDoor]       -> playerTranslatedTile <> blueDoorTile
       [Player, RedButton]      -> playerTranslatedTile <> redButtonTile
       [Player, BlueButton]     -> playerTranslatedTile <> blueButtonTile
       [Player, ExitOpened]     -> playerTranslatedTile <> openedExitTile
       [Player, RedDoorOpened]  -> playerTranslatedTile <> openedRedDoorTile
       [Player, BlueDoorOpened] -> playerTranslatedTile <> openedBlueDoorTile
       _                        -> blank)
  where
    playerTranslatedTile = playerTile mv

-- | -------------------------------- Position Handling --------------------------------
--
-- | Checks if coordinates belong to the floor's set of coordinates.
--   Implemented with an explicit check row by row
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
isBlueButtonCoord coords = coords `elem` blueButtonsCoords

blueDoorsCoords :: [Coords]
blueDoorsCoords = [(15, 1), (19, 1), (3, 17), (9, 15)]

redButtonsCoords :: [Coords]
redButtonsCoords = [(17, 17), (13, 7), (1, 11)]

isRedButtonCoord :: Coords -> Bool
isRedButtonCoord coords = coords `elem` redButtonsCoords

redDoorsCoords :: [Coords]
redDoorsCoords = [(17, 13), (11, 11), (5, 9)]

-- | Opens door by given button's index.
--   Implementation relates on tree-sets to store only unique values
openDoor ::
     Int -- ˆ 'index' must be valid number in 0 .. (length doorsCoords)
  -> [Coords] -- ˆ 'doorsCoords' are either blue or red doors' coordinates
  -> Set.Set Coords -- ˆ 'openedDoorsCoords' are either opened blue or red doors' coordinates
  -> Set.Set Coords
openDoor index doorsCoords = Set.insert $ doorsCoords !! index

-- | Opens blue door by given button's index.
--   Implementation relates on tree-sets to store only unique values
openBlueDoor ::
     Int -- ˆ 'index' must be valid number in 0 .. (length blueDoorsCoords)
  -> Set.Set Coords -- ˆ 'openedDoorsCoords' are currently opened blue doors' coordinates
  -> Set.Set Coords
openBlueDoor index = openDoor index blueDoorsCoords

-- | Opens red door by given button's index.
--   Implementation relates on tree-sets to store only unique values
openRedDoor ::
     Int -- ˆ 'index' must be valid number in 0 .. (length blueDoorsCoords)
  -> Set.Set Coords -- ˆ 'openedDoorsCoords' are currently opened blue doors' coordinates
  -> Set.Set Coords
openRedDoor index = openDoor index redDoorsCoords

-- | Checks if all red and blue doors are opened
isExitOpened ::
     Set.Set Coords -- ˆ Set of opened red doors
  -> Set.Set Coords -- ˆ Set of opened blue doors
  -> Bool
isExitOpened redDoorsOpened blueDoorsOpened =
  length redDoorsOpened + length blueDoorsOpened == 7

-- | Checks if all red and blue doors are opened and player has reached the door
isGameFinished ::
     Coords -- ˆ Current player's coordinate
  -> Set.Set Coords -- ˆ Set of opened red doors
  -> Set.Set Coords -- ˆ Set of opened blue doors
  -> Bool
isGameFinished (f, s) redDoorsOpened blueDoorsOpened =
  f == 7 && s == 19 && isExitOpened redDoorsOpened blueDoorsOpened

-- | Produces map's state for the current cell in the map by given coordinates and the game state.
--   Returns coordinates for presentation, all tiles on the given position and last player's movement
myLevelMap ::
     Coords -- ˆ Coordinate to render
  -> GameState -- ˆ Current game state
  -> (Coords, [Tile], PlayerMovement)
myLevelMap (f, s) (GameState (playerF, playerS) mv redDoorsOpened blueDoorsOpened)
  | areCoordsEqToPlayer && inBlueButtons = playerAndBlueButton
  | areCoordsEqToPlayer && inRedButtons = playerAndRedButton
  | areCoordsEqToPlayer && inBlueOpenedDoords = playerAndBlueDoorOpened
  | areCoordsEqToPlayer && inRedOpenedDoords = playerAndRedDoorOpened
  | areCoordsEqToPlayer && inBlueDoorsCoords = playerAndBlueDoor
  | areCoordsEqToPlayer && inRedDoorsCoords = playerAndRedDoor
  | areCoordsEqToPlayer && isExit && isExitReallyOpened = playerAndOpenedExit
  | areCoordsEqToPlayer && isExit = playerAndExit
  | areCoordsEqToPlayer && isFloor (f, s) = playerAndFloor
  | inBlueButtons = blueButton
  | inRedButtons = redButton
  | inBlueOpenedDoords = blueDoorOpened
  | inRedOpenedDoords = redDoorOpened
  | inBlueDoorsCoords = blueDoor
  | inRedDoorsCoords = redDoor
  | isExit && isExitReallyOpened = openedExit
  | isExit = exit
  | isFloor (f, s) = flor
  | otherwise = wall
  where
    areCoordsEqToPlayer = f == playerF && s == playerS
    inBlueButtons = (f, s) `elem` blueButtonsCoords
    inRedButtons = (f, s) `elem` redButtonsCoords
    inBlueOpenedDoords = Set.member (f, s) blueDoorsOpened
    inRedOpenedDoords = Set.member (f, s) redDoorsOpened
    inBlueDoorsCoords = (f, s) `elem` blueDoorsCoords
    inRedDoorsCoords = (f, s) `elem` redDoorsCoords
    isExit = f == 7 && s == 19
    isExitReallyOpened = isExitOpened redDoorsOpened blueDoorsOpened
    position = (f, -s)
    res arr = (position, arr, mv)
    playerAndBlueButton = res [Player, BlueButton]
    playerAndRedButton = res [Player, RedButton]
    playerAndBlueDoorOpened = res [Player, BlueDoorOpened]
    playerAndRedDoorOpened = res [Player, RedDoorOpened]
    playerAndBlueDoor = res [Player, BlueDoor]
    playerAndRedDoor = res [Player, RedDoor]
    playerAndOpenedExit = res [Player, ExitOpened]
    playerAndExit = res [Player, Exit]
    playerAndFloor = res [Player, Floor]
    blueButton = res [BlueButton]
    redButton = res [RedButton]
    blueDoorOpened = res [BlueDoorOpened]
    redDoorOpened = res [RedDoorOpened]
    blueDoor = res [BlueDoor]
    redDoor = res [RedDoor]
    openedExit = res [ExitOpened]
    exit = res [Exit]
    flor = res [Floor]
    wall = res [Wall]

-- | -------------------------------- Map Generation --------------------------------
--
row :: Int -> Row
row ind = map (\i -> (ind, gameFieldLen - i)) [0 .. gameFieldLen]

matrix :: Matrix
matrix = map (\i -> row (gameFieldLen - i)) [0 .. gameFieldLen]

rowPicture :: Row -> GameState -> Picture
rowPicture rw gameState =
  foldl (<>) blank (map (tilePic . (`myLevelMap` gameState)) rw)

matrixPicture :: Matrix -> GameState -> Picture
matrixPicture mtx gameState =
  foldl (<>) blank (map (`rowPicture` gameState) mtx)

-- | -------------------------------- Game Composition --------------------------------
--
-- | Composes game screens according to the given game state.
--   If game is finished, produces 'victory screen', otherwise - 'game screen'
levelMapPicture :: GameState -> Picture
levelMapPicture (GameState (playerF, playerS) mv redDoorsOpened blueDoorsOpened) =
  if isGameFinished (playerF, playerS) redDoorsOpened blueDoorsOpened
    then victoryScreenPic
    else matrixPicture
           matrix
           (GameState (playerF, playerS) mv redDoorsOpened blueDoorsOpened)

backgroundPic :: Picture
backgroundPic = colored purple (solidRectangle 100 100)

-- | Formats text according to the desired (by myself) format
gameStyledText ::
     Double -- ˆ X-scale
  -> Double -- ˆ Y-scale
  -> TextStyle -- ˆ Text style
  -> Text.Text -- ˆ Message
  -> Picture
gameStyledText x y style text =
  scaled x y $ colored white $ styledLettering style (NamedFont "Pristina") text

startScreenPic :: Picture
startScreenPic =
  withPaddingFromCenter
    Column
    (-2)
    [ gameStyledText 2 2 Bold "Xscape the Room"
    , gameStyledText 0.75 0.75 Italic "Press 'Space' to start"
    ] <>
  backgroundPic

victoryScreenPic :: Picture
victoryScreenPic = gameStyledText 2 2 Bold "Victory!" <> backgroundPic

-- | Composes game screens according to the given game state.
--   If game is about to start, produces 'start screen';
--   if game is finished -'victory screen';
--   otherwise - 'game screen'
levelMapPictureWithStartScreen :: WithStartScreen GameState -> Picture
levelMapPictureWithStartScreen (GameOn game) = levelMapPicture game
levelMapPictureWithStartScreen StartScreen   = startScreenPic

handleEvents ::
     Event -- ˆ Event to handle
  -> GameState -- ˆ Current game state
  -> GameState
handleEvents event (GameState (playerF, playerS) mv redDoorsOpened blueDoorsOpened) =
  case event of
    KeyPress "Up" ->
      if isGameNotFinished && isFloor (playerF, playerS - 1)
        then upped
        else currentState
    KeyPress "Down" ->
      if isGameNotFinished && isFloor (playerF, playerS + 1)
        then downed
        else currentState
    KeyPress "Left" ->
      if isGameNotFinished && isFloor (playerF - 1, playerS)
        then lefted
        else currentState
    KeyPress "Right" ->
      if isGameNotFinished && isFloor (playerF + 1, playerS)
        then righted
        else currentState
    KeyPress "Enter" ->
      if isGameNotFinished
        then if isBlueButtonCoord (playerF, playerS)
               then blueButtonOpened
               else if isRedButtonCoord (playerF, playerS)
                      then redButtonOpened
                      else currentState
        else currentState
    _ -> currentState
  where
    isGameNotFinished =
      not (isGameFinished (playerF, playerS) redDoorsOpened blueDoorsOpened)
    currentState =
      GameState (playerF, playerS) mv redDoorsOpened blueDoorsOpened
    upped = GameState (playerF, playerS - 1) UpMv redDoorsOpened blueDoorsOpened
    downed =
      GameState (playerF, playerS + 1) DownMv redDoorsOpened blueDoorsOpened
    lefted =
      GameState (playerF - 1, playerS) LeftMv redDoorsOpened blueDoorsOpened
    righted =
      GameState (playerF + 1, playerS) RightMv redDoorsOpened blueDoorsOpened
    buttonOpened = GameState (playerF, playerS) mv
    blueButtonOpened =
      buttonOpened
        redDoorsOpened
        (openBlueDoor
           (Maybe.fromJust $ List.elemIndex (playerF, playerS) blueButtonsCoords)
           blueDoorsOpened)
    redButtonOpened =
      buttonOpened
        (openRedDoor
           (Maybe.fromJust $ List.elemIndex (playerF, playerS) redButtonsCoords)
           redDoorsOpened)
        blueDoorsOpened

-- | Handles 'Esc' pressing, returning activity to its initial state
onEscPressedHandler :: ActivityOf world -> ActivityOf world
onEscPressedHandler activity initWorld eventHandler =
  activity initWorld resetableEventHandler
  where
    resetableEventHandler event world =
      case event of
        KeyPress "Esc" -> initWorld
        other          -> eventHandler other world

-- | Handles both 'Esc' (to return to the start screen)
--   and 'Space' (to start game) buttons
withStartScreen ::
     ActivityOf (WithStartScreen world)
  -> world
  -> (Event -> world -> world)
  -> (WithStartScreen world -> Picture)
  -> IO ()
withStartScreen startScreenActivity initWorld eventHandler =
  onEscPressedHandler startScreenActivity StartScreen restartableEventHandler
  where
    restartableEventHandler event world =
      case world of
        StartScreen ->
          case event of
            KeyPress " " -> GameOn initWorld
            _            -> StartScreen
        GameOn game -> GameOn (eventHandler event game)

main :: IO ()
main =
  withStartScreen
    debugActivityOf
    initialGameState
    handleEvents
    levelMapPictureWithStartScreen
  where
    initialGameState = GameState (1, 1) RightMv Set.empty Set.empty
