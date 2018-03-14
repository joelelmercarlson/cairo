{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

  import System.Environment
  import System.Exit
  import System.IO

  import Control.Arrow
  import Control.Concurrent
  import Control.Monad.Random
  import Control.Monad.Reader
  import Data.Colour.RGBSpace
  import Data.Colour.RGBSpace.HSV
  import Data.Foldable            (for_)
  import Data.List                (nub)
  import Data.Semigroup           ((<>))
  import Data.Time.Clock.POSIX
  import Graphics.Rendering.Cairo
  import Linear.V2
  import Linear.Vector
  import qualified Numeric.Noise.Perlin as P
  import Text.Printf

  import Colors

  data World = World
    {
    worldWidth    :: Int
    , worldHeight :: Int
    , worldSeed   :: Int
    , worldScale  :: Double
    }

  type Generate a = RandT StdGen (ReaderT World Render) a

  -- | renderSketch creates the drawing
  renderSketch :: Generate ()
  renderSketch = do
    quads <- genQuadGrid
    noisyQuads <- traverse quadAddNoise quads

    fillScreen eggshell 1.0
    cairo $ setLineWidth 0.15

    for_ noisyQuads $ \quad -> do
      strokeOrFill <- weighted [(fill, 0.4), (stroke, 0.6)]
      color <- uniform
        [ teaGreen
        , vividTangerine
        , englishVermillion
        , darkGunmetal
        , brandOrange
        ]
      cairo $ do
        renderQuad quad
        color 1.0 *> strokeOrFill

  -- | int main(argc, argv[])
  main = do
    xs <- getArgs

    seed <- round . (*1000) <$> getPOSIXTime

    let stdGen   = mkStdGen seed
        width    = 60 :: Int
        height   = 60 :: Int
        scaleAmt = 20 :: Double
        scaledWidth  = round $ fromIntegral width  * scaleAmt
        scaledHeight = round $ fromIntegral height * scaleAmt

    surface <- createImageSurface FormatARGB32 scaledWidth scaledHeight

    let world = World width height seed scaleAmt

    void
      . renderWith surface
      . flip runReaderT world
      . flip runRandT stdGen
      $ do
        cairo $ scale scaleAmt scaleAmt
        renderSketch

    printf "Generating Art...\n"

    surfaceWriteToPNG surface
      $ "images/seed/"
      <> show seed <> "-" <> show (round scaleAmt :: Int) <> ".png"
    surfaceWriteToPNG surface "images/latest.png"

  -- | ---------------------------------------------------------------
  -- | Lift a Cairo action into a Generate action
  cairo :: Render a -> Generate a
  cairo = lift . lift

  -- | getSize 
  getSize :: Num a => Generate (a,a)
  getSize = do
    (w, h) <- asks (worldWidth &&& worldHeight)
    pure (fromIntegral w, fromIntegral h)

  -- | fillScreen
  fillScreen :: (Double -> Render a) -> Double -> Generate ()
  fillScreen color opacity = do
    (w, h) <- getSize @Double
    cairo $ do
      rectangle 0 0 w h
      color opacity *> fill
      
  -- A --- B
  -- |     |
  -- C --- D
  data Quad = Quad
    {
    quadA   :: V2 Double
    , quadB :: V2 Double
    , quadC :: V2 Double
    , quadD :: V2 Double
    } deriving (Eq, Ord)

  -- | fromIntegralVector
  fromIntegralVector :: V2 Int -> V2 Double
  fromIntegralVector (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

  -- | genQuadGrid
  genQuadGrid :: Generate [Quad]
  genQuadGrid = do
    (w, h) <- getSize @Int
    vectors <- replicateM 800 $ do
      v <- V2 <$> getRandomR (3, w `div` 2 - 3) <*> getRandomR (3, h `div` 2 - 3)
      pure $ v ^* 2
    pure . nub . flip map vectors $ \v ->
      let v' = fromIntegralVector v
      in Quad v' (v' ^+^ V2 0.0 1.5) (v' ^+^ V2 1.5 1.5) (v' ^+^ V2 1.5 0.0)

  -- | renderClosedPath
  renderClosedPath :: [V2 Double] -> Render ()
  renderClosedPath [] = pure ()
  renderClosedPath (V2 x y:vs) = do
    newPath
    moveTo x y
    for_ vs $ \v -> let V2 x' y' = v in lineTo x' y'
    closePath

  -- | renderQuad
  renderQuad :: Quad -> Render ()
  renderQuad Quad{..} = renderClosedPath [quadA, quadB, quadC, quadD]

  -- | quadAddNoise
  quadAddNoise :: Quad -> Generate Quad
  quadAddNoise Quad{..} = do
    perlinSeed <- fromIntegral <$> asks worldSeed
    let perlinOctaves     = 5
        perlinScale       = 0.1
        perlinPersistance = 0.5
        perlinNoise = P.perlin (round perlinSeed) perlinOctaves perlinScale perlinPersistance
        perlin2d (V2 x y) = P.noiseValue perlinNoise (x + perlinSeed, y + perlinSeed, perlinSeed) - 0.5
        addNoise v = let noise = perlin2d v in v ^+^ V2 (noise / 5) (noise / 8)
    pure $ Quad
      (addNoise quadA)
      (addNoise quadB)
      (addNoise quadC)
      (addNoise quadD)
