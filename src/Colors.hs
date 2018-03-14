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

  brandOrange :: Double -> Render ()
  brandOrange = hsva a b c
    where (a,b,c) = hsvView RGB {channelRed = 232, channelGreen = 119, channelBlue = 34 }

  accessibleOrange :: Double -> Render ()
  accessibleOrange = hsva  a b c
    where (a,b,c) = hsvView RGB {channelRed = 194, channelGreen = 86, channelBlue = 8 }

  brandGold :: Double -> Render ()
  brandGold = hsva a b c
    where (a,b,c) = hsvView RGB {channelRed = 242, channelGreen = 180, channelBlue = 17 }

  gray1 :: Double -> Render ()
  gray1 = hsva a b c
    where (a,b,c) = hsvView RGB {channelRed = 40, channelGreen = 42, channelBlue = 46 }

  gray2 :: Double -> Render ()
  gray2 = hsva a b c
    where (a,b,c) = hsvView RGB {channelRed = 67, channelGreen = 68, channelBlue = 72 }

  gray3 :: Double -> Render ()
  gray3 = hsva a b c
    where (a,b,c) = hsvView RGB {channelRed = 99, channelGreen = 102, channelBlue = 106 }

  gray4 :: Double -> Render ()
  gray4 = hsva a b c
    where (a,b,c) = hsvView RGB {channelRed = 136, channelGreen = 139, channelBlue = 140 }

  gray5 :: Double -> Render ()
  gray5 = hsva a b c
    where (a,b,c) = hsvView RGB {channelRed = 177, channelGreen = 179, channelBlue = 179 }

  gray6 :: Double -> Render ()
  gray6 = hsva a b c
    where (a,b,c) = hsvView RGB {channelRed = 208, channelGreen = 208, channelBlue = 206 }

  gray7 :: Double -> Render ()
  gray7 = hsva a b c
    where (a,b,c) = hsvView RGB {channelRed = 239, channelGreen = 239, channelBlue = 238 }

