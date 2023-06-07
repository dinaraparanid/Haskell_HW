{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

type Time = Double

-- | --------------------- Math Utilitis ---------------------

-- | Floores number and converts it back to double.
-- | In other words, removes mantissa from double
-- | Example: 5.25 -> 5.00

flooredAndIntegrated :: Double -> Double
flooredAndIntegrated d = fromIntegral (floor d)

-- | Gets mantissa (part after float point) from double
-- | Example: 5.25 -> 0.25

mantissa :: Double -> Double
mantissa d = d - flooredAndIntegrated d

-- | Calculates percent from the given number.
-- | Note that from must not be equal to zero
-- | Example: x = 1; from = 8 -> 12.5

percentOf :: Double -> Double -> Double
percentOf x from = x / (from / 100)

-- | Calculates part (percent / 100) from the given number.
-- | Note that from must not be equal to zero
-- | Example: x = 1; from = 8 -> 0.125

partOf :: Double -> Double -> Double
partOf x from = (percentOf x from) / 100

-- | --------------------- Time Utilitis ---------------------

-- | Calculates time, limited with the given 'from' modula.
-- | In other words, time will be looped, when it reaches 'from'
-- | Example: t = 33.5; from = 8 -> 1.5

limitedTimeFrom :: Time -> Time -> Time
limitedTimeFrom t from = (t - ((flooredAndIntegrated (t / from)) * from))

-- | Calculates time for tree's grow animation.
-- | Produced value is in range [0..8]

treeGrowthTime :: Time -> Time
treeGrowthTime t = limitedTimeFrom t 8

-- | --------------------- Tree Components ---------------------

-- | Generates picture of a growing segment.
-- | Animation is changing according to the given time.
-- | Time should be in range [0..1]

growingSegment :: Time -> Picture
growingSegment t = solidPolygon [
    ((-partOf t 8) / 3, 0),
    ((partOf t 8) / 3, 0),
    ((partOf t 8) / 4, mantissa t),
    ((-partOf t 8) / 4, mantissa t)
  ]

-- | Generates picture of a growed subtree.
-- | Animation is changing according to the given time.
-- | Time should be in range [1..8].
-- |
-- | P.S. Да, функция высшего порядка, здесь будет уместнее
-- | притвориться умным, чтобы не дублировать себя (принцип DRY)

subtree :: Double -> (Double -> Picture) -> Picture
subtree t treeFn = segment <> translated 0 1 (leftBranch <> rightBranch)
    where
      segment = solidPolygon [
          ((-partOf t 8) / 3, 0),
          ((partOf t 8) / 3, 0),
          ((partOf t 8) / 4, 1),
          ((-partOf t 8) / 4, 1)
        ]
      leftBranch = rotated (pi / 10) (treeFn (t - 1))
      rightBranch = rotated (-pi / 10) (treeFn (t - 1))

-- | Generates picture of a green leaf.
-- | Animation is changing according to the given time.
-- | Time should be in range [0..1]

leaf :: Double -> Picture
leaf t = (translated 0 (mantissa t) (colored green (solidCircle ((mantissa t) / 8))))

-- | --------------------- Trees ---------------------

-- | Generates picture of a tree.
-- | Animation is changing according to the given time.
-- | For time not greater than 1 -> growing segment
-- | Otherwise -> subtree

tree :: Double -> Picture
tree t
  | t <= 1 = growingSegment t
  | otherwise = subtree t tree

-- | Generates picture of a tree with leaves on growing segments.
-- | Animation is changing according to the given time.
-- | For time not greater than 1 -> growing segment with leaves
-- | Otherwise -> subtree

greenTree :: Double -> Picture
greenTree t
  | t <= 1 = leaf t <> growingSegment t
  | otherwise = subtree t greenTree

-- | Generates picture of a tree that
-- | controls its growth according to the time.
-- | On all steps except last one generates nude tree (without leaves).
-- | On the last step generates tree with leaves

animatedTree :: Double -> Picture
animatedTree t
  | (treeGrowthTime t) >= 7 = greenTree (treeGrowthTime t)
  | otherwise = tree (treeGrowthTime t)

main :: IO ()
main = animationOf animatedTree
