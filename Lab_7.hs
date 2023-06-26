{-# OPTIONS_GHC -fdefer-typed-holes -Wall #-}

solveApplicative :: Int -> [(Int, Int, Int)]
solveApplicative n =
  map snd $ filter (\(v, _) -> v == n) $ allLHS [0 .. 9] [0 .. 9] [0 .. 9]
  where
    allLHS solX solY solZ = filter distinct $ [lhs] <*> solX <*> solY <*> solZ
    lhs x y z = (x + y * z, (x, y, z))
    distinct (_, (x, y, z)) = x /= y && x /= z && y /= z

solveMonadic :: Int -> [(Int, Int, Int)]
solveMonadic n = do
  x <- [0 .. 9]
  y <- filter (/= x) [0 .. 9]
  z <- filter (\zz -> zz /= x && zz /= y) [0 .. 9]
  if x + y * z == n
    then return (x, y, z)
    else []

solveListComprehension :: Int -> [(Int, Int, Int)]
solveListComprehension n =
  [ (x, y, z)
  | x <- [0 .. 9]
  , y <- filter (/= x) [0 .. 9]
  , z <- filter (\zz -> zz /= x && zz /= y) [0 .. 9]
  , x + y * z == n
  ]

main :: IO ()
main = print $ solveListComprehension 22
