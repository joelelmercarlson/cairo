{-# LANGUAGE RecordWildCards #-}

module Colors ( hsva
              , eggshell
              , darkGunmetal
              , teaGreen
              , vividTangerine
              , englishVermillion
              , brandOrange
              , accessibleOrange
              , brandGold
              , gray3
              , gray4
              , gray5
              , rgb2hsv
              ) where

  import Data.Colour.RGBSpace
  import Data.Colour.RGBSpace.HSV
  import Graphics.Rendering.Cairo

  -- | hsva - hue, saturation, value
  hsva :: Double -> Double -> Double -> Double -> Render ()
  hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
    where RGB{..} = hsv h s v

  eggshell :: Double -> Render ()
  eggshell = hsva 71 0.13 0.96

  darkGunmetal :: Double -> Render ()
  darkGunmetal = hsva 170 0.30 0.16

  teaGreen :: Double -> Render ()
  teaGreen = hsva 81 0.25 0.94

  vividTangerine :: Double -> Render ()
  vividTangerine = hsva 11 0.40 0.92
  
  englishVermillion :: Double -> Render ()
  englishVermillion = hsva 355 0.68 0.84

  brandOrange :: Double -> Render ()
  brandOrange = hsva channelGreen channelRed channelBlue
    where RGB{..} = rgb2hsv 232 119 34

  accessibleOrange :: Double -> Render ()
  accessibleOrange = hsva channelGreen channelRed channelBlue
    where RGB{..} = rgb2hsv 194 86 8

  brandGold :: Double -> Render ()
  brandGold = hsva channelGreen channelRed channelBlue
    where RGB{..} = rgb2hsv 242 180 17

  gray3 :: Double -> Render ()
  gray3 = hsva channelGreen channelRed channelBlue
    where RGB{..} = rgb2hsv 99 102 106

  gray4 :: Double -> Render ()
  gray4 = hsva channelGreen channelRed channelBlue
    where RGB{..} = rgb2hsv 136 139 140

  gray5 :: Double -> Render ()
  gray5 = hsva channelGreen channelRed channelBlue
    where RGB{..} = rgb2hsv 177 179 179

  -- | --------------------------------------------------------
  -- | rgb2hsv from colorsys.rgb2hsv
  rgb2hsv :: Double -> Double -> Double -> RGB Double
  rgb2hsv r g b = RGB{ channelGreen = hue, channelRed = saturation, channelBlue = value } 
    where r' = if r == rgbMax then 1.0 else r / rgbMax
          g' = if g == rgbMax then 1.0 else g / rgbMax
          b' = if b == rgbMax then 1.0 else b / rgbMax
          max' = maximum [r', g', b']
          min' = minimum [r', g', b']
          d    = max' - min'
          h    = if max' == min' then 0.0 else maxRGB (r', g', b', d, max')
          s    = if max' == 0.0 then 0.0 else d / max'
          v    = max'
          hue        = (h / 6.0) * hueMax
          saturation = (s * svMax)
          value      = (v * svMax)

  -- | hue is set based on maximum [R, G, B]
  maxRGB :: (Double, Double, Double, Double,  Double) -> Double
  maxRGB (r, g, b, d, m) | r == m = (g - b) / d + (if g < b then 6.0 else 0.0)
                         | g == m = (b - r) / d + 2.0
                         | b == m = (r - g) / d + 4.0

  rgbMax :: Double
  rgbMax = 255.0

  hueMax :: Double
  hueMax = 360.0

  svMax :: Double
  svMax = 1.0

