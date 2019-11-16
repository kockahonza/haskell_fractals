module Main where

import Data.Maybe
import Data.Complex
import Data.Char
import Data.List

import Graphics.Gloss hiding (line)
import Graphics.Gloss.Juicy
import Codec.Picture

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

colorFunc :: Func a b -> Coloring b -> (a -> a -> PixelRGB8)
colorFunc f c = curry (c . uncurry f)

redLinearColoring :: (RealFrac a) => a -> Coloring a
redLinearColoring m n = PixelRGB8 (round (255 * (n / m))) 0 0

colorListColoring :: (Integral a) => [PixelRGB8] -> Coloring a
colorListColoring colors n = colors !! m
    where
        m = fromIntegral n `mod` length colors

--------------------------------------------------------------------------------

showDynamicImage :: DynamicImage -> IO ()
showDynamicImage im = display (InWindow "Nice Window" (800, 800) (0, 0)) white pic
    where
        pic = fromMaybe Blank (fromDynamicImage im)

imageFromFractal :: (RealFrac a) => Fractal a b -> DynamicImage
imageFromFractal (Fractal _ (xMin, xStep, xMax) (yMin, yStep, yMax) func col) = ImageRGB8 $ generateImage f
        (round ((xMax - xMin) / xStep)) (round ((yMax - yMin) / yStep))
    where
        f :: Int -> Int -> PixelRGB8
        f x y = (colorFunc func col) (xMin + xStep * fromIntegral x) (yMin + yStep * fromIntegral y)

savePngFractal :: (RealFrac a) => Fractal a b -> IO ()
savePngFractal fractal = do
    let im = imageFromFractal fractal
    savePngImage ("images/" ++ show fractal ++ ".png") im

--------------------------------------------------------------------------------

juliaNum :: (RealFloat a, Integral b) => Complex a -> b -> a -> a -> Complex a
juliaNum c 1 x y = x :+ y
juliaNum c n x y = (juliaNum c (n-1) x y)**2 + c

getJuliaNumFunc :: (RealFloat a, Integral b) => Complex a -> b -> Func a a
getJuliaNumFunc c n = curry $ magnitude . uncurry (juliaNum c n)

jetJuliasStepsFunc :: (RealFloat a, Integral b) => Complex a -> a -> a -> a -> b
jetJuliasStepsFunc c m x y = fst $ head $ filter ((>= m) . snd) [(n, magnitude (juliaNum c n x y)) | n <- [1..]]

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
    (jetJuliasStepsFunc c m)
    col


range = (-2, 0.001, 2)

getJulNumBasicRed :: Int -> Fractal Double Double
getJulNumBasicRed n = getJulNumFractal (1 :+ 1) n range (redLinearColoring 2) "redLinear 2"

getJulNumBasicCol :: Int -> Fractal Double Double
getJulNumBasicCol n = getJulNumFractal (1 :+ 1) n range (colorListColoring myColors . round) "myColors coloring"

julNum1Col = getJulNumFractal ((-0.79) :+ 0.15) 6 range (colorListColoring myColors . round) "myColors coloring"
julNum1Red = getJulNumFractal ((-0.79) :+ 0.15) 6 range (redLinearColoring 2) "redLinear 2"

julSteps1Red = getJulStepsFractal ((-0.79) :+ 0.15) 2 range (redLinearColoring 10 . fromIntegral) "redLinear 10"
julSteps1Col = getJulStepsFractal ((-0.79) :+ 0.15) 2 range (colorListColoring myColors) "myColors coloring"

julNum2Col = getJulNumFractal (0.28 :+ 0.008) 6 range (colorListColoring myColors . round) "myColors coloring"
julNum2Red = getJulNumFractal (0.28 :+ 0.008) 6 range (redLinearColoring 2) "redLinear 2"

julSteps2Red = getJulStepsFractal (0.28 :+ 0.008) 2 range (redLinearColoring 1 . fromIntegral) "redLinear 1"
julSteps2Col = getJulStepsFractal (0.28 :+ 0.008) 2 range (colorListColoring myColors) "myColors coloring"

main :: IO ()
main = do
    -- mapM_ (\n -> savePngFractal (getJulNumBasicRed n)) [1..6]
    -- mapM_ (\n -> savePngFractal (getJulNumBasicCol n)) [1..6]
    -- savePngFractal julNum1Red
    -- savePngFractal julNum1Col
    -- savePngFractal julNum2Red
    -- savePngFractal julNum2Col

    -- savePngFractal julSteps1Red
    -- savePngFractal julSteps1Col
    savePngFractal julSteps2Red
    -- savePngFractal julSteps2Col
