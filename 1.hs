{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

type Tile = Int
type Coords = (Int, Int)
type Row = [Coords]
type Matrix = [Row]

floorTile :: Picture
floorTile = colored yellow (solidRectangle 0.95 0.95)

wallTile :: Picture
wallTile = colored black (solidRectangle 0.95 0.95)

doorTileGen :: Int -> Color -> Picture
doorTileGen i c
  | i == 1 = colored black (solidRectangle 0.19 0.19)
  | i `mod` 2 == 1 = ((doorTileGen (i - 1) c)) <> ((colored black (solidRectangle (0.19 * (fromIntegral i))  (0.19 * (fromIntegral i)))))
  | otherwise = (doorTileGen (i - 1) c) <> (colored c (solidRectangle (0.19 * (fromIntegral i))  (0.19 * (fromIntegral i))))

doorTile :: Color -> Picture
doorTile c = doorTileGen 5 c

redDoorTile :: Picture
redDoorTile = doorTile red

blueDoorTile :: Picture
blueDoorTile = doorTile blue

buttonTile :: Color -> Picture
buttonTile c = colored c (solidCircle 0.3)

blueButtonTile :: Picture
blueButtonTile = buttonTile blue

redButtonTile :: Picture
redButtonTile = buttonTile red

tile :: (Coords, Tile) -> Picture
tile ((f, s), 0) = translated (fromIntegral f) (fromIntegral s) floorTile
tile ((f, s), 1) = translated (fromIntegral f) (fromIntegral s) wallTile
tile ((3, -2), 6) = translated 3.0 (-2.0) redDoorTile
tile ((6, -2), 3) = translated 6.0 (-2.0) (blueButtonTile <> wallTile)
tile ((2, -8), 3) = translated 2.0 (-8.0) (blueButtonTile <> floorTile)
tile ((6, -9), 5) = translated 6.0 (-9.0) (redButtonTile <> wallTile)
tile ((8, -2), 5) = translated 8.0 (-2.0) (redButtonTile <> floorTile)

myLevelMap :: Coords -> (Coords, Tile)
myLevelMap (f, s)
  | f == 6 && s == 2 = ((f, -s), 3)
  | f == 2 && s == 8 = ((f, -s), 3)
  | f == 8 && s == 2 = ((f, -s), 5)
  | f == 6 && s == 9 = ((f, -s), 5)
  | f == 3 && s == 2 = ((f, -s), 6)
  | f >= 1 && f <= 5 && s >= 1 && s <= 3 = ((f, -s), 0)
  | f >= 7 && f <= 9 && s >= 1 && s <= 9 = ((f, -s), 0)
  | f >= 1 && f <= 5 && s >= 5 && s <= 9 = ((f, -s), 0)
  | otherwise = ((f, -s), 1)
  
coordinate :: Int -> Int -> Coords
coordinate i q = (i, q)

rowGen :: Int -> Int -> Row
rowGen i rowInd
  | i == 0 = [(rowInd, 10)]
  | otherwise = (rowInd, (10 - i)) : (rowGen (i - 1) rowInd)

row :: Int -> Row
row ind = rowGen 10 ind

matrixGen :: Int -> Matrix
matrixGen i
  | i == 0 = [row 10]
  | otherwise = row (10 - i) : matrixGen (i - 1)
  
matrix :: Matrix
matrix = matrixGen 10

rowToPictures :: Row -> [Picture]
rowToPictures row = map (tile) (map myLevelMap row)

combineRowPicturesGen :: Int -> [Picture] -> Picture
combineRowPicturesGen i pics
  | i == 0 = pics !! 0
  | otherwise = (pics !! ((length pics) - i)) <> (combineRowPicturesGen (i - 1) pics)
  
combineRowPictures :: [Picture] -> Picture
combineRowPictures pics = combineRowPicturesGen (length pics) pics

rowPicture :: Row -> Picture
rowPicture row = combineRowPictures (rowToPictures row)

matrixPictureGen :: Int -> Matrix -> Picture
matrixPictureGen i mtx
  | i == 0 = rowPicture (mtx !! 0)
  | otherwise = (rowPicture (mtx !! ((length mtx) - i))) <> (matrixPictureGen (i - 1) mtx)

myLevelMapPicture :: Picture
myLevelMapPicture = matrixPictureGen 10 matrix

main :: IO ()
main = drawingOf myLevelMapPicture
