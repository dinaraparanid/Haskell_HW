import CodeWorld

type Time = Double

type Vec = (Double, Double)

type DistanceFromSol = Double

type Radius = Double

type Distance = Double

type Speed = Double

type Angle = Double

type Position = (Double, Double)

type PlanetMovementParams = (Planet, Time, Radius, Speed, DistanceFromSol)

data Planet = Earth | Mars

data SpaceObject = Planet | Asteroid | Sol

earthRadius :: Radius
earthRadius = 0.06371

marsRadius :: Radius
marsRadius = 0.03389

asteroidRadius :: Radius
asteroidRadius = 0.006

solRadius :: Radius
solRadius = 0.6

earthDistance :: Distance
earthDistance = 1.5184

marsDistance :: Distance
marsDistance = 2.5065

earthSpeed :: Speed
earthSpeed = 0.00029

marsSpeed :: Speed
marsSpeed = 0.00012

orbitPic :: DistanceFromSol -> Picture
orbitPic d = colored grey (circle d)

planetPic :: Planet -> Position -> Picture
planetPic Earth (x, y) = translated x y (colored blue (solidCircle earthRadius))
planetPic Mars (x, y) = translated x y (colored red (solidCircle marsRadius))

planetWithOrbitPic :: Planet -> Position -> Picture
planetWithOrbitPic Earth pos = (orbitPic earthDistance) <> (planetPic Earth pos)
planetWithOrbitPic Mars pos = (orbitPic marsDistance) <> (planetPic Mars pos)

solPic :: Picture
solPic = colored yellow (solidCircle solRadius)

asteroidPic :: Position -> Picture
asteroidPic (x, y) = translated x y (colored grey (solidCircle asteroidRadius))

fieldPic :: Picture
fieldPic = colored black (solidRectangle 30 30)

moveAngle :: Time -> Radius -> Speed -> Angle
moveAngle t r s = moved / way * 360
  where moved = t * s
        way = 2 * pi * r

position :: DistanceFromSol -> Angle -> Position
position d a = (d * cos(a), d * sin(a))

planetsMovementParams :: Time -> [PlanetMovementParams]
planetsMovementParams t = [
    (Earth, t, earthRadius, earthSpeed, earthDistance),
    (Mars, t, marsRadius, marsSpeed, marsDistance)
 ]

-- Map/Reduce to convert planets' data to planets' pictures and combine them

planetsPic :: Time -> Picture
planetsPic t = foldl1 (<>)
  (map (\(planet, pos) -> planetWithOrbitPic planet pos)
    (map (\(planet, time, rad, speed, dist) -> (planet, position dist (moveAngle time rad speed)))
      (planetsMovementParams t)))

systemPic :: Double -> Picture
systemPic t = planetsPic t <> solPic <> fieldPic 

main :: IO ()
main = animationOf systemPic
