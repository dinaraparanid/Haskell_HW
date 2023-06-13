{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
import Data.Text (Text)

data Tree a label = Empty | Leaf a | Node label (Tree a label) (Tree a label)

sampleTree :: Tree Text Text
sampleTree =
  Node "*"
    (Node "+"
      (Leaf "1")
      (Node "f" Empty (Leaf "3"))
    )
    (Node "/"
      (Node "+"
        (Node "–" (Leaf "4") (Leaf "5"))
        Empty
      )
      (Node "g"
        (Leaf "7")
        (Leaf "8")
      )
    )

height :: Tree a label -> Int
height Empty = 1
height (Leaf _) = 1
height (Node _ left right) = 1 + max (height left) (height right)

countNodes :: Tree a label -> Int
countNodes Empty = 0
countNodes (Leaf _) = 0
countNodes (Node _ left right) = 1 + countNodes left + countNodes right

mapLeaves :: (a -> b) -> Tree a label -> Tree b label
mapLeaves _ Empty = Empty
mapLeaves trans (Leaf x) = Leaf (trans x)
mapLeaves trans (Node lab left right) =
  Node lab (mapLeaves trans left) (mapLeaves trans right)

mapLabels :: (label -> b) -> Tree a label -> Tree a b
mapLabels _ Empty = Empty
mapLabels _ (Leaf x) = Leaf x
mapLabels trans (Node lab left right) =
  Node (trans lab) (mapLabels trans left) (mapLabels trans right)

treeToList :: Tree a label -> [a]
treeToList Empty = []
treeToList (Leaf x) = [x]
treeToList (Node lab left right) = concat [treeToList left, treeToList right]

listToTree :: [a] -> Tree a ()
listToTree [] = Empty
listToTree [x] = Leaf x
listToTree (x : xs) = Node () (Leaf x) (listToTree xs)

balancedTreeFromList :: [a] -> Tree a ()
balancedTreeFromList [] = Empty
balancedTreeFromList [x] = Leaf x
balancedTreeFromList list = Node () (balancedTreeFromList firstSlice) (balancedTreeFromList secondSlice)
  where
    firstSliceLen = (length list) `div` 2
    firstSlice = take firstSliceLen list
    secondSlice = drop firstSliceLen list

labelPicSize = 0.75 :: Double

treePic :: Tree Picture Picture -> Picture
treePic Empty = translated 0 (-0.25) (circle 0.25)
treePic (Leaf pic) = translated 0 (-0.75) (pic <> (rectangle 1.5 1.5))

treePic (Node lab left right) = segment <> leftSubtree <> rightSubtree
  where
    segment = translated 0 (-labelPicSize) (circle labelPicSize <> lab)

    leftNodes = fromIntegral (countNodes left)
    rightNodes = fromIntegral (countNodes right)

    leftHeight = fromIntegral (height left)
    rightHeight = fromIntegral (height right)

    leftYPosition = (labelPicSize * 4) + rightHeight - min leftHeight rightHeight
    rightYPosition = (labelPicSize * 4) + leftHeight - min leftHeight rightHeight

    leftXPosition = (labelPicSize * 2) + rightNodes
    rightXPosition = (labelPicSize * 2) + leftNodes

    leftLine = polyline [(0, -(labelPicSize * 2)), (-leftXPosition, -leftYPosition)]
    rightLine = polyline [(0, -(labelPicSize * 2)), (rightXPosition, -rightYPosition)]

    leftSegment = translated (-leftXPosition) (-leftYPosition) (treePic left)
    rightSegment = translated rightXPosition (-rightYPosition) (treePic right)

    leftSubtree = leftLine <> leftSegment
    rightSubtree = rightLine <> rightSegment

main :: IO ()
main = drawingOf (treePic (mapLabels lettering (mapLeaves lettering sampleTree)))
