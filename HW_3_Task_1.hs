{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module SimpleElevator where

import           CodeWorld
import qualified Data.List as List

-- | ----------------------- Types And Aliases -----------------------
type Time = Double

-- | Elevator's Y-position
type ElevatorPosition = Double

-- | Button states for elevator.
--   Corresponds to actions in Elevator's FSM system
data ElevatorButtonState
  = Up
  | Down
  | Stop

instance Eq ElevatorButtonState where
  (==) Up Up     = True
  (==) Down Down = True
  (==) Stop Stop = True
  (==) _ _       = False

-- | Elevator's moving state.
--   Corresponds to state in Elevator's FSM system
data ElevatorState
  = MovingUp
  | MovingDown
  | Idle

instance Eq ElevatorState where
  (==) MovingUp MovingUp     = True
  (==) MovingDown MovingDown = True
  (==) Idle Idle             = True
  (==) _ _                   = False

-- | Elevator's system state.
--   Serves as a global configuration for Elevator's FSM system
data ElevatorSystemState =
  ElevatorSystemState ElevatorState ElevatorPosition

-- | Enum for 'withPaddingFromCenter'
--   to layout items in desirable order
--   (either Row or Column layouts)
data ContainerView
  = Row
  | Column

-- | ----------------------- Solutions and utilities -----------------------
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
       (\(i, x) -> translated (fromIntegral i * padding) 0 (trans x))
       (enumerate items))
mapWithPaddingFromCenter Column padding trans items =
  foldl
    (<>)
    blank
    (map
       (\(i, x) -> translated 0 (fromIntegral i * padding) (trans x))
       (enumerate items))

-- | Idling elevator at zero position
initialElevatorSystemState :: ElevatorSystemState
initialElevatorSystemState = ElevatorSystemState Idle 0

-- | All buttons in one line
solution1 :: IO ()
solution1 =
  drawingOf (mapWithPaddingFromCenter Row 5 elevatorButtonPic [Up, Down, Stop])

-- | Handling events, elevator jumping
solution2 :: IO ()
solution2 =
  debugActivityOf
    initialElevatorSystemState
    updateElevatorState
    callbackSystemPic
  where
    updateElevatorState event = nextElevatorState (eventToButtonState event)

-- | Same as 'solution2' but with
--   additional FSM layer of abstraction
solution3 :: IO ()
solution3 =
  interactiveFSM
    initialElevatorSystemState
    (==)
    nextElevatorAction
    eventToButtonState
    callbackSystemPic

-- | Final version with event and time handling
solution4 :: IO ()
solution4 =
  interactiveSystem
    initialElevatorSystemState
    (==)
    nextElevatorAction
    eventToButtonState
    recomposeElevatorSystem
    animatedSystemPic

-- | ----------------------------- Buttons -----------------------------
elevatorButtonBackgroundPic :: Picture
elevatorButtonBackgroundPic =
  colored gray (solidCircle 0.8) <> colored black (solidCircle 1)

elevatorButtonUpTrianglePic :: Picture
elevatorButtonUpTrianglePic =
  solidPolygon [(-0.5, -0.5), (0, -0.375), (0.5, -0.5), (0, 0.625)]

elevatorButtonDownTrianglePic :: Picture
elevatorButtonDownTrianglePic =
  solidPolygon [(-0.5, 0.5), (0, 0.375), (0.5, 0.5), (0, -0.625)]

elevatorButtonPic :: ElevatorButtonState -> Picture
elevatorButtonPic Up =
  elevatorButtonUpTrianglePic <> elevatorButtonBackgroundPic
elevatorButtonPic Down =
  elevatorButtonDownTrianglePic <> elevatorButtonBackgroundPic
elevatorButtonPic Stop =
  colored red (scaled 0.5 0.5 (lettering "STOP")) <> elevatorButtonBackgroundPic

-- | ----------------------------- Controller -----------------------------
elevatorControllerBackgroundPic :: Picture
elevatorControllerBackgroundPic =
  colored gray (solidRectangle 1.75 5.75) <> colored black (solidRectangle 2 6)

elevatorControllerUpTrianglePic :: Picture
elevatorControllerUpTrianglePic =
  solidPolygon [(-0.65, -0.75), (0, -0.55), (0.65, -0.75), (0, 1.25)]

elevatorControllerDownTrianglePic :: Picture
elevatorControllerDownTrianglePic =
  solidPolygon [(-0.65, 0.75), (0, 0.55), (0.65, 0.75), (0, -1.25)]

elevatorControllerPic :: ElevatorState -> Picture
elevatorControllerPic Idle =
  translated 0 1.25 elevatorControllerUpTrianglePic <>
  translated 0 (-1.25) elevatorControllerDownTrianglePic <>
  elevatorControllerBackgroundPic
elevatorControllerPic MovingUp =
  translated 0 1.25 (colored red elevatorControllerUpTrianglePic) <>
  translated 0 (-1.25) elevatorControllerDownTrianglePic <>
  elevatorControllerBackgroundPic
elevatorControllerPic MovingDown =
  translated 0 1.25 elevatorControllerUpTrianglePic <>
  translated 0 (-1.25) (colored red elevatorControllerDownTrianglePic) <>
  elevatorControllerBackgroundPic

-- | ----------------------------- Elevator System -----------------------------
elevatorPic :: Picture
elevatorPic =
  scaled 2 2 (lettering "🚶") <>
  colored white (solidRectangle 2 2.75) <> colored black (solidRectangle 2.25 3)

-- | Elevator's system that handles only keyboard actions, but not time changing.
--   In other words, elevator jumps on every new state receivement
callbackSystemPic :: ElevatorSystemState -> Picture
callbackSystemPic (ElevatorSystemState Idle _) =
  withPaddingFromCenter
    Row
    5
    [ translated 0 0 elevatorPic
    , elevatorControllerPic Idle
    , withPaddingFromCenter
        Row
        (-2.5)
        [elevatorButtonPic Up, elevatorButtonPic Down]
    ]
callbackSystemPic (ElevatorSystemState MovingUp _) =
  withPaddingFromCenter
    Row
    5
    [translated 0 5 elevatorPic, elevatorControllerPic MovingUp] <>
  translated 8.75 0 (elevatorButtonPic Stop)
callbackSystemPic (ElevatorSystemState MovingDown _) =
  withPaddingFromCenter
    Row
    5
    [translated 0 (-5) elevatorPic, elevatorControllerPic MovingDown] <>
  translated 8.75 0 (elevatorButtonPic Stop)

-- | Elevator's system that handles both keyboard actions and time changing
animatedSystemPic :: ElevatorSystemState -> Picture
animatedSystemPic (ElevatorSystemState Idle pos) =
  withPaddingFromCenter
    Row
    5
    [ translated 0 pos elevatorPic
    , elevatorControllerPic Idle
    , withPaddingFromCenter
        Row
        (-2.5)
        [elevatorButtonPic Up, elevatorButtonPic Down]
    ]
animatedSystemPic (ElevatorSystemState MovingUp pos) =
  withPaddingFromCenter
    Row
    5
    [translated 0 pos elevatorPic, elevatorControllerPic MovingUp] <>
  translated 8.75 0 (elevatorButtonPic Stop)
animatedSystemPic (ElevatorSystemState MovingDown pos) =
  withPaddingFromCenter
    Row
    5
    [translated 0 pos elevatorPic, elevatorControllerPic MovingDown] <>
  translated 8.75 0 (elevatorButtonPic Stop)

-- | Recomposes elevator's position by the given time.
--   After reaching |pos| = 5, elevator changes its movement direction
recomposeElevatorSystem ::
     ElevatorSystemState -- ˆ Current system's state
  -> ElevatorPosition -- ˆ Offset to move elevator
  -> ElevatorSystemState
recomposeElevatorSystem (ElevatorSystemState MovingUp pos) dt =
  if pos + dt >= 5
    then ElevatorSystemState MovingDown (pos - dt)
    else ElevatorSystemState MovingUp (pos + dt)
recomposeElevatorSystem (ElevatorSystemState MovingDown pos) dt =
  if pos - dt <= -5
    then ElevatorSystemState MovingUp (pos + dt)
    else ElevatorSystemState MovingDown (pos - dt)
recomposeElevatorSystem sys _ = sys

-- | ----------------------------- FSM implementation -----------------------------
-- | Gets next FSM state according to the given action.
--   In case if action wasn't produced or found, returns current state
applyAction ::
     Maybe a -- ˆ Action to apply
  -> (a -> a -> Bool) -- ˆ Action equality checker
  -> (s -> [(a, s)]) -- ˆ State transitions
  -> s -- ˆ Current state
  -> s
applyAction (Just nextAct) eq trans curState =
  case List.find (\(act, _) -> eq act nextAct) (trans curState) of
    Nothing             -> curState
    Just (_, nextState) -> nextState
applyAction Nothing _ _ curState = curState

-- | Gets all possible next Elevator System's actions
--   that are allowed to happed after current system's state
nextElevatorAction ::
     ElevatorSystemState -> [(ElevatorButtonState, ElevatorSystemState)]
nextElevatorAction (ElevatorSystemState Idle pos) =
  [ (Up, ElevatorSystemState MovingUp pos)
  , (Down, ElevatorSystemState MovingDown pos)
  ]
nextElevatorAction (ElevatorSystemState _ pos) =
  [(Stop, ElevatorSystemState Idle pos)]

-- | Converts key pressing event to 'ElevatorButtonState'
--   Returns 'Nothing' if event is not related to Up/Down/Enter buttons
eventToButtonState :: Event -> Maybe ElevatorButtonState
eventToButtonState (KeyPress "Up")    = Just Up
eventToButtonState (KeyPress "Down")  = Just Down
eventToButtonState (KeyPress "Enter") = Just Stop
eventToButtonState _                  = Nothing

-- | Gets next ElevatorSystemState after applying a button's action
--   In case if action wasn't provided, returns same system's state
nextElevatorState ::
     Maybe ElevatorButtonState -- ˆ Action to apply
  -> ElevatorSystemState -- ˆ Current system's state
  -> ElevatorSystemState
nextElevatorState (Just mbNextAct) curSys =
  applyAction (Just mbNextAct) (==) nextElevatorAction curSys
nextElevatorState Nothing sys = sys

interactiveFSM ::
     system -- ˆ Initial FSA state
  -> (a -> a -> Bool) -- ˆ Action equality checker
  -> (system -> [(a, system)]) -- ˆ State transitions
  -> (Event -> Maybe a) -- ˆ Event to action transformation
  -> (system -> Picture) -- ˆ System picture
  -> IO ()
interactiveFSM curState eq trans eventToAct =
  debugActivityOf curState updateState
  where
    updateState event = applyAction (eventToAct event) eq trans

interactiveSystem ::
     system -- ˆ Initial FSA state
  -> (a -> a -> Bool) -- ˆ Action equality checker
  -> (system -> [(a, system)]) -- ˆ State transitions
  -> (Event -> Maybe a) -- ˆ Event to action transformation
  -> (system -> Time -> system) -- ˆ System recomposition
  -> (system -> Picture) -- ˆ System picture
  -> IO ()
interactiveSystem initSys eq trans eventToAct recomposeSys =
  debugActivityOf initSys updateState
  where
    updateState event curSys =
      case event of
        TimePassing dt -> recomposeSys curSys dt
        other          -> applyAction (eventToAct other) eq trans curSys

main :: IO ()
main = solution4
