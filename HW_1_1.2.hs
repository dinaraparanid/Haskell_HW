{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- | Run with code.world: https://code.world/haskell#P8L_JGFAPS3s85DslpCh7mg

type Time = Double

-- | ---------------------- Traffic Light States ----------------------

-- | States of the traffic light
--
-- * '0' - active car green light (drive); active pedestrian red light (stop)
-- * '1' - active yellow light (prepare to stop); active pedestrian red light (stop)
-- * '2' - active red light (stop); active pedestrian green light (go)
-- * '3' - active yellow and red lights (prepare to go); blinking (green) walking light (move fast)
-- * '4' - active yellow and red lights (prepare to go); blinking (black) walking light (move fast)

type SystemState = Int

-- | Car's states of the traffic light
--
-- * '0' - active car green light (drive)
-- * '1' - active yellow light (prepare to stop)
-- * '2' - active red light (stop)
-- * '3' - active yellow and red lights (prepare to go)

type CarState = Int

-- | Pedestrian's states of the traffic light
--
-- * '0' - active pedestrian red light (stop)
-- * '1' - active pedestrian green light (go)
-- * '2' - blinking (black) pedestrian light (move fast)

type PedestrianState = Int

-- | Bicycle's states of the traffic light
--
-- * '0' - active bicycle red light (stop)
-- * '1' - active bicycle green light (go)
-- * '2' - blinking (black) bicycle light (move fast)

type BicycleState = Int

-- | ---------------------- Traffic Light Utils ----------------------

-- | Draws a circle for traffic lights given color and XY-offsets

trafficCircle :: Color -> Double -> Double -> Picture
trafficCircle c x y = translated x y (colored c (solidCircle 1))

-- | Draws a light (colored) circle for traffic lights given color and XY-offsets

lightCircle :: Color -> Double -> Double -> Picture
lightCircle c x y = trafficCircle c x y

-- | Draws a black circle for traffic lights given XY-offset.

inactiveLight :: Double -> Double -> Picture
inactiveLight x y = trafficCircle black x y

-- | ---------------------- Basic Traffic Lights ----------------------

-- | Draws a red circle for traffic lights and XY-offset

redLight :: Double -> Double -> Picture
redLight x y = lightCircle red x y

-- | Draws a yellow circle for traffic lights and XY-offset

yellowLight :: Double -> Double -> Picture
yellowLight x y = lightCircle yellow x y

-- | Draws a green circle for traffic lights and XY-offset

greenLight :: Double -> Double -> Picture
greenLight x y = lightCircle green x y

-- | ---------------------- Car Active Lights ----------------------

-- | Draws a red circle for cars' traffic lights

carRedLight :: Picture
carRedLight = redLight 0 1.2

-- | Draws a yellow circle for cars' traffic lights

carYellowLight :: Picture
carYellowLight = yellowLight 0 (-1.2)

-- | Draws a green circle for cars' traffic lightsca

carGreenLight :: Picture
carGreenLight = greenLight 0 (-3.6)

-- | ---------------------- Walking Active Lights ----------------------
-- | Supposed to be used with both pedestrians and bicycles, cause they walk

-- | Draws a red circle for walkers' traffic lights with given X-offset

walkingRedLight :: Double -> Picture
walkingRedLight x = lightCircle red x 1.2

-- | ---------------------- Pedestrian Active Lights ----------------------

-- | Draws a red circle for pedestrians' traffic lights

pedestrianRedLight :: Picture
pedestrianRedLight = walkingRedLight 4

-- | Draws a green circle for pedestrians' traffic lights

pedestrianGreenLight :: Picture
pedestrianGreenLight = (translated 4 (-1.2) (lettering "\x1F6B6")) <> (lightCircle green 4 (-1.2))

-- | ---------------------- Bicycle Active Lights ----------------------

-- | Draws a red circle for bicycles' traffic lights

bicycleRedLight :: Picture
bicycleRedLight = walkingRedLight 8

-- | Draws a green circle for bicycles' traffic lights

bicycleGreenLight :: Picture
bicycleGreenLight = (translated 8 (-1.2) (lettering "\x1F6B2")) <> (lightCircle green 8 (-1.2))

-- | ---------------------- Inactive Car Lights ----------------------

inactiveCarRedLight :: Picture
inactiveCarRedLight = inactiveLight 0 1.2

inactiveCarYellowLight :: Picture
inactiveCarYellowLight = inactiveLight 0 (-1.2)

inactiveCarGreenLight :: Picture
inactiveCarGreenLight = inactiveLight 0 (-3.6)

-- | ---------------------- Inactive Walking Lights ----------------------
-- | Supposed to be used with both pedestrians and bicycles, cause they walk

inactiveWalkingRedLight :: Double -> Picture
inactiveWalkingRedLight x = inactiveLight x 1.2

inactiveWalkingGreenLight :: Double -> Picture
inactiveWalkingGreenLight x = inactiveLight x (-1.2)

-- | ---------------------- Inactive Pedestrian Lights ----------------------

inactivePedestrianRedLight :: Picture
inactivePedestrianRedLight = inactiveWalkingRedLight 4

inactivePedestrianGreenLight :: Picture
inactivePedestrianGreenLight = inactiveWalkingGreenLight 4

-- | ---------------------- Inactive Bicycle Lights ----------------------

inactiveBicycleRedLight :: Picture
inactiveBicycleRedLight = inactiveWalkingRedLight 8

inactiveBicycleGreenLight :: Picture
inactiveBicycleGreenLight = inactiveWalkingGreenLight 8

-- | ---------------------- Traffic Light Frames ----------------------

carFrame :: Picture
carFrame = translated 0 (-1.2) (colored grey (rectangle 2.5 7.5))

walkingFrame :: Double -> Picture
walkingFrame x = translated x 0 (colored grey (rectangle 2.5 5))

pedestrianFrame :: Picture
pedestrianFrame = walkingFrame 4

bicycleFrame :: Picture
bicycleFrame = walkingFrame 8

-- | ---------------------- Traffic Light States ----------------------

-- | States of the traffic light:
--
-- * '0' - active car green light (drive) (st 0); active pedestrian red light (stop) (st 0)
-- * '1' - active yellow light (prepare to stop) (st 1); active pedestrian red light (stop) (st 0)
-- * '2' - active red light (stop) (st 2); active pedestrian green light (go) (st 1)
-- * '3' - active yellow and red lights (prepare to go) (st 3); blinking (green) pedestrian green light (move fast) (st 1)
-- * '4' - active yellow and red lights (prepare to go) (st 3); blinking (black) pedestrian green light (move fast) (st 2)

-- | ---------------------- Car States ----------------------

zeroStateCar :: Picture
zeroStateCar = carFrame <> inactiveCarRedLight <> inactiveCarYellowLight <> carGreenLight

firstStateCar :: Picture
firstStateCar = carFrame <> inactiveCarRedLight <> carYellowLight <> inactiveCarGreenLight

secondStateCar :: Picture
secondStateCar = carFrame <> carRedLight <> inactiveCarYellowLight <> inactiveCarGreenLight

thirdStateCar :: Picture
thirdStateCar = carFrame <> carRedLight <> carYellowLight <> inactiveCarGreenLight

-- | ---------------------- Pedestrian States ----------------------

zeroStatePedestrian :: Picture
zeroStatePedestrian = pedestrianFrame <> pedestrianRedLight <> inactivePedestrianGreenLight

firstStatePedestrian :: Picture
firstStatePedestrian = pedestrianFrame <> inactivePedestrianRedLight <> pedestrianGreenLight

secondStatePedestrian :: Picture
secondStatePedestrian = pedestrianFrame <> inactivePedestrianRedLight <> inactivePedestrianGreenLight

-- | ---------------------- Bicycle States ----------------------

zeroStateBicycle :: Picture
zeroStateBicycle = bicycleFrame <> bicycleRedLight <> inactiveBicycleGreenLight

firstStateBicycle :: Picture
firstStateBicycle = bicycleFrame <> inactiveBicycleRedLight <> bicycleGreenLight

secondStateBicycle :: Picture
secondStateBicycle = bicycleFrame <> inactiveBicycleRedLight <> inactiveBicycleGreenLight

-- | ---------------------- System States ----------------------

systemToCarState :: SystemState -> CarState
systemToCarState 0 = 0
systemToCarState 1 = 1
systemToCarState 2 = 2
systemToCarState 3 = 3
systemToCarState 4 = 3

systemToPedestrianState :: SystemState -> PedestrianState
systemToPedestrianState 0 = 0
systemToPedestrianState 1 = 0
systemToPedestrianState 2 = 1
systemToPedestrianState 3 = 1
systemToPedestrianState 4 = 2

systemToBicycleState :: SystemState -> BicycleState
systemToBicycleState 0 = 0
systemToBicycleState 1 = 0
systemToBicycleState 2 = 1
systemToBicycleState 3 = 1
systemToBicycleState 4 = 2

carStateToPic :: CarState -> Picture
carStateToPic 0 = zeroStateCar
carStateToPic 1 = firstStateCar
carStateToPic 2 = secondStateCar
carStateToPic 3 = thirdStateCar

pedestrianStateToPic :: PedestrianState -> Picture
pedestrianStateToPic 0 = zeroStatePedestrian
pedestrianStateToPic 1 = firstStatePedestrian
pedestrianStateToPic 2 = secondStatePedestrian

bicycleStateToPic :: BicycleState -> Picture
bicycleStateToPic 0 = zeroStateBicycle
bicycleStateToPic 1 = firstStateBicycle
bicycleStateToPic 2 = secondStateBicycle

state :: Int -> Picture
state s = (carStateToPic (systemToCarState s)) <> (pedestrianStateToPic (systemToPedestrianState s)) <> (bicycleStateToPic (systemToBicycleState s))

zeroState :: Picture
zeroState = state 0

firstState :: Picture
firstState = state 1

secondState :: Picture
secondState = state 2

thirdState :: Picture
thirdState = state 3

fourthState :: Picture
fourthState = state 4

-- | Produces traffic light image from the given TL's state
-- | For every state there are 3 lights which can be either active
-- | (with light color) or inactive (with black color) according to the next strategy:
--
-- * '0' - active car green light (drive); active pedestrian red light (stop)
-- * '1' - active yellow light (prepare to stop); active pedestrian red light (stop)
-- * '2' - active red light (stop); active pedestrian green light (go)
-- * '3' - active yellow and red lights (prepare to go); blinking pedestrian green light (move fast)
-- * '4' - active yellow and red lights (prepare to go); blinking (black) pedestrian green light (move fast)

trafficLights :: SystemState -> Picture
trafficLights 0 = zeroState
trafficLights 1 = firstState
trafficLights 2 = secondState
trafficLights 3 = thirdState
trafficLights 4 = fourthState

-- | Produces traffic light image with the given animation:
--
-- * '1..3' - active green light (go)
-- * '4' - active yellow light (prepare to stop)
-- * '5..7' - active red light (stop)
-- * '8' - active yellow and red lights (prepare to go)
--
-- Animation is looped, meaning that states will go
-- one by one in the same order eternally

trafficController :: Double -> Picture
trafficController t
  | floor t `mod` 8 < 3 = trafficLights 0
  | floor t `mod` 8 == 3 = trafficLights 1
  | floor t `mod` 8 > 3 && floor (t) `mod` 8 < 7 = trafficLights 2
  | t - fromIntegral (floor t) < 0.25 = trafficLights 3
  | t - fromIntegral (floor t) >= 0.25 && t - fromIntegral (floor t) < 0.5 = trafficLights 4
  | t - fromIntegral (floor t) >= 0.5 && t - fromIntegral (floor t) < 0.75 = trafficLights 3
  | otherwise = trafficLights 4

main :: IO ()
main =  animationOf trafficController
