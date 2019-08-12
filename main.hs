--module Main(main) where

import System.Random
import Data.Fixed
import Graphics.Gloss
--import Graphics.Gloss.Export
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Geometry.Angle

type Angle  = Float
type Coords = (Float , Float)
type Size   = Float
type Size'  = (Size , Size)
type Step   = Int


title :: String
title = "My Fractals"

background :: Color
background = myWhite

coloursMondel :: [Color]
coloursMondel = [mixColors stepp (100 - stepp) myYellow myRed    | stepp <- [0,d..100]] ++ 
                [mixColors stepp (100 - stepp) myRed    myWhite  | stepp <- [0,d..100]] ++ 
                [mixColors stepp (100 - stepp) myWhite  myPurple | stepp <- [0,d..100]] ++ 
                [mixColors stepp (100 - stepp) myPurple myGreen  | stepp <- [0,d..100]] ++ [black]
                  where
                    d = 100 / (stepsMandel/4)

coordsCloud, coordsSnow, coordsSun, coordsTree :: Coords
coordsCloud  = ( 200 ,  300)
coordsSnow   = ( 200 ,  300)
coordsSun    = (-200 ,  300)
coordsTree   = (-100 , -200)

coordsWindow :: (Int , Int)
coordsWindow = (  10 ,   10)

periodSnow, periodSun, periodTree :: Float
periodSnow = 3
periodSun  = 3
periodTree = 10

sizeMondel, sizeSnow, sizeSun, sizeTree :: Size
sizeMondel = 1000
sizeSnow   = 0.5
sizeSun    = 100
sizeTree   = 50

size'Mondel :: Size'
size'Mondel = (100 , 100)

sizeWindow :: Int
sizeWindow = 800

stepsSnow, stepsSun, stepsTree :: Int
stepsMandel = 80
stepsSnow   = 4
stepsSun    = 15
stepsTree   = 10

myBlue, myBrown, myGreen, myRed, myWhite, myYellow :: Color
myBlue   = makeColorI 129 212 250 255
myBrown  = makeColorI 123  63  30 255
myGray   = makeColorI 200 200 200 255
myGreen  = makeColorI 118 255   3 255
myPurple = makeColorI 138  43 226 255
myRed    = makeColorI 255   0   0 255
myWhite  = makeColorI 255 255 255 255
myYellow = makeColorI 255 255   0 255

--Use a parabolic function to smooth out the change in angle-- Time elapsed -> Resulting Angle
getAngle :: Float -> Angle
--getAngle time = -3.6*t^2 + 36*t         --0-90-0
--getAngle time = -2*t^2 + 20*t + 20      --20-70-20
getAngle time = (-1.2)*t^2 + 12*t + 30    --30-60-30
                  where t = time `mod'` periodTree

--Define house-like shape for simple tree-- Bottom side -> Left triangle's angle -> Tree
solidTree :: Size -> Angle -> Picture
solidTree side angle = polygon [(0         , 0),
                                (0         , side),
                                (side*co^2 , side*(1+si*co)),
                                (side      , side),
                                (side      , 0)]
                          where
                            si = sin (degToRad angle)
                            co = cos (degToRad angle)

--Define shape for simple sun-- Size -> Sun
--NOTE: Actual side is 3x given one
solidSun :: Size -> Picture
solidSun side = pictures [
                    polygon [(0-si , 0-i),
                             (0    , h-i),
                             (si   , 0-i)] ,
                    polygon [(0-si , i  ),
                             (si   , i  ),
                             (0    , s-i)]]
                  where
                    si = side / 2
                    s = side / 3 * (-0.86602540378)
                    t = side * 0.57735026919
                    i = t / 2
                    h = side * 0.86602540378

--Fade colours for tree-- Steps left -> Resulting colour
nextColourTree :: Step -> Color
nextColourTree stepsLeft = mixColors (100 - stepp) (stepp) myGreen myBrown
                            where stepp = (fromIntegral stepsLeft) * 100 / (fromIntegral stepsTree)

--Fade colours for sun-- Steps left -> Resulting colour
nextColourSun :: Step -> Color
nextColourSun stepsLeft = mixColors (100 - stepp) (stepp) myRed myYellow
                            where
                              stepp = (fromIntegral stepsLeft) * 100 / (fromIntegral stepsSun)

coloursSun :: [Color]
coloursSun = cycle $ tail list ++ tail (reverse list)
            where
              list  = [mixColors (100 - stepp) (stepp) myRed myYellow | stepp <- percs]
              percs = [(fromIntegral steppp) * 100 / (fromIntegral stepsSun) | steppp <- [0..stepsSun]]


--Sums two vectors-- Coords 1 -> Coords2 -> Resulting coords
sumTuples :: Coords -> Coords -> Coords
sumTuples (u,v) (x,y) = (u+x , v+y)

--Translate a picture by tuple-- Tuple -> Picture -> Resulting picture
myTranslate :: Coords -> Picture -> Picture
myTranslate (x,y) pic = translate x y pic

--Rotate Point around centre-- Point -> Angle -> Resulting point
--NOTE: Positive angle is anticlockwise
rotatePoint :: Coords -> Angle -> Coords
rotatePoint (x,y) ang = (x*(cos(degToRad ang)) - y*(sin(degToRad ang)),
                         x*(sin(degToRad ang)) + y*(cos(degToRad ang)))

coordsFinderLeft :: Coords -> Size -> Angle -> Coords
coordsFinderLeft (x,y) newSize newAngle = (x,y) `sumTuples` (rotatePoint (0 , newSize) newAngle)

coordsFinderRight :: Coords -> Size -> Angle -> Angle -> Coords
coordsFinderRight (x,y) newSize newAngle angle = (x,y) `sumTuples` (rotatePoint (newSize*co^2 , newSize*(1 + co*si)) newAngle)
  where
    si = sin (degToRad angle)
    co = cos (degToRad angle)

drawLeft :: Step -> Coords -> Size -> Angle -> Angle -> [Picture]
drawLeft 0 _ _ _ _ = []
drawLeft left oldCoords oldSize oldAngle angle =  color newColour (myTranslate newCoords (rotate ((-1)*newAngle) (solidTree newSize angle)))
                                              :  (drawLeft newLeft newCoords newSize newAngle angle)
                                              ++ (drawRight newLeft newCoords newSize newAngle angle)
  where
    newLeft = left - 1
    newSize = oldSize*co
    newAngle = oldAngle + angle
    newCoords = coordsFinderLeft oldCoords oldSize oldAngle
    newColour = nextColourTree left
    --si = sin (degToRad angle)
    co = cos (degToRad angle)

drawRight :: Step -> Coords -> Size -> Angle -> Angle -> [Picture]
drawRight 0 _ _ _ _ = []
drawRight left oldCoords oldSize oldAngle angle = color newColour (myTranslate newCoords (rotate ((-1)*newAngle) (solidTree newSize angle)))
                                              :  [pictures (drawLeft newLeft newCoords newSize newAngle angle)]
                                              ++ [pictures (drawRight newLeft newCoords newSize newAngle angle)]
  where
    newLeft = left - 1
    newSize = oldSize*si
    newAngle = oldAngle - (90 - angle)
    newCoords = coordsFinderRight oldCoords oldSize oldAngle angle
    newColour = nextColourTree left
    si = sin (degToRad angle)
    --co = cos (degToRad angle)

getGrass :: Float -> Float -> [Float] -> Picture
getGrass w h ran = color green $ pictures [translate x 0 $ line [(0,0),(0,h+y)] | (x,y) <- grasss]
  where
    grasss = zip [0,3..w] (map (\x -> x*15 + 5) ran)

escaperMondel :: (Float , Float) -> (Float , Float) -> Int -> Color
escaperMondel (x,y) (nx , ny) n
                      | n == length coloursMondel - 1 = last coloursMondel
                      | nxx + nyy > 4.0 = coloursMondel !! n
                      | otherwise = escaperMondel (x,y) (nxx - nyy + x , 2*nx*ny + y) (n+1)
                          where
                            nxx = nx^2
                            nyy = ny^2

getMondel :: Picture
getMondel = pictures $  [color (escaperMondel (x*d-2,y*d) (x*d-2,y*d) 0) $ pictures [translate (x) (-y) (rectangleSolid 1 1) , translate (x) (y) (rectangleSolid 1 1)]
                                                          | x <- [0..sm] , y <- [0..sm/2]]
                                                          -- | x <- [-2,d-2..0.5] , y <- [0,d..1.25]]
--getMondel = last [(x,y) | x <- [-2,d-2..0.5] , y <- [-1.25,d-1.25..1.25]]
                          where
                            sm = sizeMondel
                            d = 2.5 / sm


--parseSnow :: Generating string -> First point -> Angle with x-axis -> Line path
parseSnow :: String -> Point -> Angle -> Path
parseSnow [] p _ = [p]
parseSnow ('-':xs)   p   ang =           parseSnow xs p             (ang + 60)
parseSnow ('+':xs)   p   ang =           parseSnow xs p             (ang - 60)
parseSnow ( _ :xs) (x,y) ang = (x , y) : parseSnow xs (newX , newY)  ang
      where
        ang' = degToRad ang
        newX = x+sizeSnow*(cos ang')
        newY = y+sizeSnow*(sin ang')

getSnow1 :: Step -> String -> String
getSnow1 0 str = str
getSnow1 steps str = getSnow1 (steps - 1) (replacerSnow1 str)

getSnow2 :: Step -> String -> String
getSnow2 0 str = str
getSnow2 steps str = getSnow2 (steps - 1) (replacerSnow2 str)

--Initial String: "A"
replacerSnow1 :: String -> String
replacerSnow1 [] = []
replacerSnow1 ('A':xs) = "A-B--B+A++AA+B-" ++ replacerSnow1 xs
replacerSnow1 ('B':xs) = "+A-BB--B-A++A+B" ++ replacerSnow1 xs
replacerSnow1 (x:xs)   = x : replacerSnow1 xs

--Initial String: "A++A++A"
replacerSnow2 :: String -> String
replacerSnow2 [] = []
replacerSnow2 ('A':xs) = "A-A++A-A" ++ replacerSnow2 xs
replacerSnow2 (x:xs)   = x : replacerSnow2 xs

--Returns x-coordinates and spawning times for snawflakes-- Random list -> Last spawning time in list -> Complete list
spawnSnow :: [Float] -> Float -> [(Float , Float)]
spawnSnow (r:rs) last = (x,last) : spawnSnow rs new
                        where
                          x   = r * 150
                          new = last + (r * periodSnow)

drawGrass :: [Float] -> Float -> Picture
drawGrass ran time = translate (-400) (-400) (getGrass 800 300 ran)

drawTree :: Step -> [Float] -> Float -> Picture
drawTree 0 rans time = pictures [color myBrown (solidTree sizeTree angle)] where angle = getAngle time
drawTree step rans time = pictures $ drawTree 0 rans time : (drawLeft step (0,0) sizeTree 0 angle) ++ (drawRight step (0,0) sizeTree 0 angle) ++ [getGrass sizeTree 0 rans]
                          where angle = getAngle time


drawCloud :: Picture
drawCloud = color myGray $ myTranslate coordsCloud $ translate 75 25 $ rectangleSolid 150 50

drawSnow :: [Float] -> Float -> Picture                            -- getSnow2 stepsSnow "A++A++A" -- getSnow1 stepsSnow "A"
drawSnow rans time = pictures [translate x (0 - 5*t^2) $ color myWhite $ line (parseSnow (getSnow2 stepsSnow "A++A++A") coordsSnow 0) | (x,t) <- params]
                      where
                        timeToGrass = sqrt $ (0.3 * snd coordsSnow)
                        params = dropWhile (\x -> snd x >= timeToGrass) $ map (\(x,t) -> (x , time -t)) listt
                        listt = takeWhile (\x -> snd x <= time) $ spawnSnow rans 0

drawSun' :: Float -> String
drawSun' time = "Time = " ++ show time ++ " ; Steps elapsed = " ++ show colourStep
    where
      newSide stepp = sizeSun * (fromIntegral stepp) / (fromIntegral stepsSun)
      colourStep = round ((time `mod'` periodSun) / (periodSun / (2*(fromIntegral stepsSun))))


drawSun :: Float -> [Picture]
--drawSun 0 _ = []
drawSun time = reverse [color ((drop colourStep coloursSun) !! stepp) $ rotate (fromIntegral (30*stepp)) (solidSun (newSide stepp)) | stepp <- [1..stepsSun]]
--drawSun stepp = (color (nextColourSun stepp) $ rotate (fromIntegral (30*stepp)) (solidSun (newSide stepp))) : drawSun (stepp - 1)
    where
      newSide stepp = sizeSun * (fromIntegral stepp) / (fromIntegral stepsSun)
      colourStep = round ((time `mod'` periodSun) / (periodSun / (2*(fromIntegral stepsSun))))

drawSky :: [Float] -> Float -> Picture
drawSky rans time = pictures [color myBlue $ translate 0 150 $ rectangleSolid 800 500 , myTranslate coordsSun $ pictures (drawSun time) , drawSnow rans time , drawCloud]



draw :: [Float] -> Float -> Picture
draw rans time = pictures $ drawSky rans time : [drawGrass rans time] ++ [myTranslate coordsTree $ drawTree stepsTree rans time]

draw _ _ = getMondel
draw' = getMondel

window :: Display
window = InWindow title (sizeWindow, sizeWindow) coordsWindow
--window = InWindow title (round sizeMondel, round sizeMondel) coordsWindow

main :: IO ()
main = do
          r <- getStdGen
          --exportPictureToPNG (round sizeMondel,round sizeMondel) white "C:\\Users\\Lorenzo\\Desktop\\127_001\\HASKELL\\FP\\Design\\ciaone.png" $ translate (-sizeMondel/2) 0 draw'
          animate window background (draw (randomRs (0, 1) r))
          --display window background  $ translate (-sizeMondel/2) 0 draw'