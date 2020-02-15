module Main where

import Data.Maybe
import Data.Complex
import Data.Char
import Data.List

import Graphics.Gloss hiding (line)
import Graphics.Gloss.Juicy
import Codec.Picture

--------------------------------------------------------------------------------
-- very hex color parsing
--------------------------------------------------------------------------------

hexStrToPix :: String -> PixelRGB8
hexStrToPix ['#', r1, r2, g1, g2, b1, b2] = PixelRGB8 (16 * hexCharToInt r1 + hexCharToInt r2) (16 * hexCharToInt g1 + hexCharToInt g2) (16 * hexCharToInt b1 + hexCharToInt b2)
    where
        hexCharToInt :: Char -> Pixel8
        hexCharToInt = fromJust . flip lookup (zip (['0'..'9'] ++ ['A'..'F']) [0..]) . toUpper

myColors :: [PixelRGB8]
myColors = [
    hexStrToPix "#845ec2",
    hexStrToPix "#d65db1",
    hexStrToPix "#ff6f91",
    hexStrToPix "#ff9671",
    hexStrToPix "#ffc75f",
    hexStrToPix "#f9f871",
    hexStrToPix "#008f7a",
    hexStrToPix "#0081cf"
           ]

--------------------------------------------------------------------------------
-- Type definitions
--------------------------------------------------------------------------------

data Fractal a b = Fractal {
    name     :: String,
    xRange   :: (a, a, a),
    yRange   :: (a, a, a),
    func     :: Func a b,
    coloring :: Coloring b
}

instance Show (Fractal a b) where
    show (Fractal name _ _ _ _) = name

type Func a b = (a -> a -> b)

type Coloring a = (a -> PixelRGB8)

--------------------------------------------------------------------------------
-- Colorings
--------------------------------------------------------------------------------

colorFunc :: Func a b -> Coloring b -> (a -> a -> PixelRGB8)
colorFunc f c = curry (c . uncurry f)

getRedLinColoring :: (RealFrac a) => a -> Coloring a
getRedLinColoring m n = PixelRGB8 (round (255 * (n / m))) 0 0

getColorListColoring :: (Integral a) => [PixelRGB8] -> Coloring a
getColorListColoring colors n = colors !! m
    where
        m = fromIntegral n `mod` length colors

--------------------------------------------------------------------------------
-- Functions for rendering the Fractals
--------------------------------------------------------------------------------

showFractal :: (RealFrac a) => Fractal a b -> IO ()
showFractal fractal = do
    let im = imageFromFractal fractal
        pic = fromMaybe Blank (fromDynamicImage im)
    display (InWindow (show fractal) (800, 800) (0, 0)) white pic

imageFromFractal :: (RealFrac a) => Fractal a b -> DynamicImage
imageFromFractal (Fractal _ (xMin, xStep, xMax) (yMin, yStep, yMax) func col) = ImageRGB8 $ generateImage f
        (round ((xMax - xMin) / xStep)) (round ((yMax - yMin) / yStep))
    where
        f :: Int -> Int -> PixelRGB8
        f x y = colorFunc func col (xMin + xStep * fromIntegral x) (yMin + yStep * fromIntegral y)

savePngFractal :: (RealFrac a) => Fractal a b -> IO ()
savePngFractal fractal = do
    let im = imageFromFractal fractal
    savePngImage ("images/" ++ show fractal ++ ".png") im

--------------------------------------------------------------------------------
-- Code for some Julian fractal images
--------------------------------------------------------------------------------

juliaNum :: (RealFloat a, Integral b) => Complex a -> b -> a -> a -> Complex a
juliaNum c 1 x y = x :+ y
juliaNum c n x y = (juliaNum c (n-1) x y)**3 + (0 :+ (-1))

getJuliaNumFunc :: (RealFloat a, Integral b) => Complex a -> b -> Func a a
getJuliaNumFunc c n = curry $ magnitude . uncurry (juliaNum c n)

getJuliaStepsFunc :: (RealFloat a, Integral b) => Complex a -> a -> a -> a -> b
getJuliaStepsFunc c m x y = fst $ head $ filter ((>= m) . snd) [(n, magnitude (juliaNum c n x y)) | n <- [1..]]

getJulNumFractal :: (RealFloat a, Show a, Integral b, Show b) => Complex a -> b -> (a, a, a) -> Coloring a -> String -> Fractal a a
getJulNumFractal c n r col colName = Fractal
    ("Num" ++ "_" ++ colName ++ "_" ++ show c ++ "_" ++ show n ++ "_" ++ show r)
    r
    r
    (getJuliaNumFunc c n)
    col

getJulStepsFractal :: (RealFloat a, Show a, Integral b, Show b) => Complex a -> a -> (a, a, a) -> Coloring b -> String -> Fractal a b
getJulStepsFractal c m r col colName = Fractal
    ("Steps" ++ "_" ++ colName ++ "_" ++ show c ++ "_" ++ show m ++ "_" ++ show r)
    r
    r
    (getJuliaStepsFunc c m)
    col

range = (-2, 0.5, 2)

getJulNumBasicRed :: Int -> Fractal Double Double
getJulNumBasicRed n = getJulNumFractal (1 :+ 1) n range (getRedLinColoring 2) "redLinear 2"

getJulNumBasicCol :: Int -> Fractal Double Double
getJulNumBasicCol n = getJulNumFractal (1 :+ 1) n range (getColorListColoring myColors . round) "myColors coloring"

julNum1Col = getJulNumFractal ((-0.79) :+ 0.15) 6 range (getColorListColoring myColors . round) "myColors coloring"
julNum1Red = getJulNumFractal ((-0.79) :+ 0.15) 6 range (getRedLinColoring 2) "redLinear 2"

julNum2Col = getJulNumFractal (0.28 :+ 0.008) 6 range (getColorListColoring myColors . round) "myColors coloring"
julNum2Red = getJulNumFractal (0.28 :+ 0.008) 6 range (getRedLinColoring 2) "redLinear 2"

julStepsBasicRed = getJulStepsFractal (2 :+ 2) 2 range (getRedLinColoring 10 . fromIntegral) "redLinear 10"

julSteps1Red = getJulStepsFractal ((-0.79) :+ 0.15) 2 range (getRedLinColoring 10 . fromIntegral) "redLinear 10"
julSteps1Col = getJulStepsFractal ((-0.79) :+ 0.15) 2 range (getColorListColoring myColors) "myColors coloring"

julSteps2Red = getJulStepsFractal (0.28 :+ 0.008) 2 range (getRedLinColoring 10 . fromIntegral) "redLinear 10"
julSteps2Col = getJulStepsFractal (0.28 :+ 0.008) 2 range (getColorListColoring myColors) "myColors coloring"

anotherColoring :: (RealFrac a) => a -> Coloring a
anotherColoring m n = PixelRGB8 (round (255 * (n / m))) 0 0

sinFunc :: (Floating a) => Func a a
sinFunc x y = abs (y - sin x)

sinFractal = Fractal
    "sinFrac_recLin_1"
    (-4, 0.001, 4)
    (-4, 0.001, 4)
    sinFunc
    (anotherColoring 1)

complexLogFunc :: (RealFloat a) => Func a a
complexLogFunc x y = magnitude (cos t :+ sin t)
    where
        t = magnitude (x :+ y)

complexLogFractal = Fractal
    "complexLogFrac_recLin_1"
    (-4, 0.001, 4)
    (-4, 0.001, 4)
    complexLogFunc
    (anotherColoring 1)

main :: IO ()
main = do
    savePngFractal complexLogFractal
    -- mapM_ (\n -> savePngFractal (getJulNumBasicRed n)) [1..6]
    -- mapM_ (\n -> savePngFractal (getJulNumBasicCol n)) [1..6]
    -- savePngFractal julNum1Red
    -- savePngFractal julNum1Col
    -- savePngFractal julNum2Red
    -- savePngFractal julNum2Col

    -- savePngFractal julStepsBasicRed
    -- savePngFractal julSteps1Red
    -- savePngFractal julSteps1Col
    -- savePngFractal julSteps2Red
    -- savePngFractal julSteps2Col
