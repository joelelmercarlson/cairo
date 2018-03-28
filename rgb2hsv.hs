#!/usr/bin/env stack
-- stack script --system-ghc --resolver lts-9.18 
module Main where

  import System.Environment
  import System.Exit
  import System.IO

  import Text.Printf

  -- | rgb2hsv - convert RGB into Hue, Saturation, Value
  main :: IO ()
  main = do
    xs <- getArgs

    let r = getv (nth 1 xs)
        g = getv (nth 2 xs)
        b = getv (nth 3 xs)
    rgb2hsv r g b

  -- | --------------------------------------------------------
  -- | nth safe chooser
  nth :: Int -> [a] -> Maybe a
  nth _ []     = Nothing
  nth 1 (x:_)  = Just x
  nth n (x:xs) = nth (n - 1) xs

  -- | getv values
  getv :: Maybe String -> Double
  getv x = case x of
             Just x  -> read x :: Double
             Nothing -> 0.0

  -- | --------------------------------------------------------
  -- | rgb2hsv from colorsys.rgb2hsv
  rgb2hsv :: Double -> Double -> Double -> IO ()
  rgb2hsv r g b = do
    let r' = if r == rgbMax then 1.0 else r / rgbMax
        g' = if g == rgbMax then 1.0 else g / rgbMax
        b' = if b == rgbMax then 1.0 else b / rgbMax
        max' = maximum [r', g', b']
        min' = minimum [r', g', b']
        d    = max' - min'
        h    = if max' == min' then 0.0 else maxRGB (r', g', b', d, max')
        s    = if max' == 0.0 then 0.0 else d / max'
        v    = max'
        hue        = (h / 6.0) * hueMax
        saturation = s
        value      = v
    printf "R=%.2f G=%.2f B=%.2f is H=%.2f S=%.2f V=%.2f\n" r g b hue saturation value

  -- | h is set based on maximum [R, G, B]
  maxRGB :: (Double, Double, Double, Double,  Double) -> Double
  maxRGB (r, g, b, d, m) | r == m = (g - b) / d + (if g < b then 6.0 else 0.0)
                         | g == m = (b - r) / d + 2.0
                         | b == m = (r - g) / d + 4.0

  rgbMax :: Double
  rgbMax = 255.0

  hueMax :: Double
  hueMax = 360.0

  svMax :: Double
  svMax = 100.0

