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
              , gray4
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

  -- 232, 119, 34
  brandOrange :: Double -> Render ()
  brandOrange = hsva 25.76 85.34 90.98

  -- 194, 86, 8
  accessibleOrange :: Double -> Render ()
  accessibleOrange = hsva 25.16 95.88 76.08

  -- 242, 180, 17
  brandGold :: Double -> Render ()
  brandGold = hsva 43.47 92.98 94.90

  -- 136, 139, 140
  gray4 :: Double -> Render ()
  gray4 = hsva 195 2.86 54.90
