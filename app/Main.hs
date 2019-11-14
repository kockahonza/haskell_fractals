module Main where

import Data.Maybe
import Data.Complex
import Data.Char

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Codec.Picture
import Data.Hex

type Func = (Float -> Float -> PixelRGB8)

showDynamicImage :: DynamicImage -> IO ()
showDynamicImage im = display (InWindow "Nice Window" (800, 800) (0, 0)) white pic
    where
        pic = fromMaybe Blank (fromDynamicImage im)

imageFromFuncInRange :: Int -> Float -> Int -> Int -> Float -> Int -> Func -> DynamicImage
imageFromFuncInRange xMin xStep xMax yMin yStep yMax f = ImageRGB8 $ generateImage offsetF
        (round (fromIntegral (xMax - xMin) / xStep)) (round (fromIntegral (yMax - yMin) / yStep))
    where
        offsetF :: Int -> Int -> PixelRGB8
        offsetF x y = f (fromIntegral xMin + xStep * fromIntegral x) (fromIntegral yMin + yStep * fromIntegral y)

--------------------------------------------------------------------------------

hexStrToPix :: String -> PixelRGB8
hexStrToPix ['#', r1, r2, g1, g2, b1, b2] = PixelRGB8 (16 * hexCharToInt r1 + hexCharToInt r2) (16 * hexCharToInt g1 + hexCharToInt g2) (16 * hexCharToInt b1 + hexCharToInt b2)
    where
        hexCharToInt :: Char -> Pixel8
        hexCharToInt = fromJust . flip lookup (zip (['0'..'9'] ++ ['A'..'F']) [0..]) . toUpper

testFunc :: Func
-- testFunc x y = PixelRGB8 (fromIntegral ((round x) `mod` 256)) (fromIntegral ((round y) `mod` 256)) 0
testFunc x y
  | abs x == 100 || abs y == 100 = PixelRGB8 0 0 0
  | otherwise = PixelRGB8 255 255 255

myColors = [
    (0, hexStrToPix "#845ec2"),
    (1, hexStrToPix "#d65db1"),
    (2, hexStrToPix "#ff6f91"),
    (3, hexStrToPix "#ff9671"),
    (4, hexStrToPix "#ffc75f"),
    (5, hexStrToPix "#f9f871"),
    (6, hexStrToPix "#008f7a"),
    (7, hexStrToPix "#0081cf")
           ]

juliaFunc :: Complex Float -> Int -> Func
-- juliaFunc c n x y = PixelRGB8 (round (mag * (255 / 1000000000))) 0 0
juliaFunc c n x y
  | mag <= 2 = hexStrToPix "#845ec2"
  | mag <= 3 = hexStrToPix "#d65db1"
  | mag <= 7 = hexStrToPix "#ff6f91"
  | logBase 10 mag <= 6 = hexStrToPix "#ff9671"
  | logBase 10 mag <= 7 = hexStrToPix "#ffc75f"
  | logBase 10 mag <= 9 = hexStrToPix "#f9f871"
  | logBase 10 mag <= 10 = hexStrToPix "#008f7a"
  | logBase 10 mag <= 12 = hexStrToPix "#0081cf"
  | otherwise = hexStrToPix "#fbeaff"
    where
        mag = magnitude (zJ c n x y)

juliaFunc2 :: Complex Float -> Func
juliaFunc2 c x y = PixelRGB8 (round ((fromIntegral n) * (255 / 10))) 0 0
-- juliaFunc2 c x y = fromMaybe (hexStrToPix "#000000") $ lookup n myColors
    where
        n :: Int
        n = fst $ head $ filter ((>= 2) . snd) [(n, magnitude (zJ c n x y)) | n <- [1..]]

zJ :: Complex Float -> Int -> Float -> Float -> Complex Float
zJ c 1 x y = x :+ y
zJ c n x y = (zJ c (n-1) x y)**2 + c

main :: IO ()
main = do
    let im  = imageFromFuncInRange (-2) 0.005 2 (-2) 0.005 2 (juliaFunc2 ((0.28) :+ 0.008))
    savePngImage "12.png" im
