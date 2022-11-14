module Canvas where

import Codec.Picture
import Data.Array
import Data.Bits ((.&.), (.|.))
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Data.Fixed (mod')
import Layout
import Shape

type RGBImage = Image PixelRGB8

data Color
  = Color
      { red :: Pixel8
      , green :: Pixel8
      , blue :: Pixel8
      }
  | ColorFn
      { colorFn :: (Shape -> Point -> Color)
      }

colorAnd :: Color -> Color -> Color
colorAnd (Color r g b) (Color r' g' b') = Color (r .&. r') (g .&. g') (b .&. b')
colorAnd _ _ = error "Cannot apply AND on a color function"

colorOr :: Color -> Color -> Color
colorOr (Color r g b) (Color r' g' b') = Color (r .|. r') (g .|. g') (b .|. b')
colorOr _ _ = error "Cannot apply OR on a color function"

instance Show Color where
  show c =
    case c of
      (Color {}) ->
        "(R: " ++
        (show $ red c) ++
        ", G: " ++ (show $ green c) ++ ", B: " ++ (show $ blue c) ++ ")"
      (ColorFn {}) -> "(Color Function)"

colorValue :: Color -> Shape -> Point -> Color
colorValue (Color r g b) _ _ = Color r g b
colorValue (ColorFn f) s p = f s p

rgb :: Color -> PixelRGB8
rgb c = PixelRGB8 (red c) (green c) (blue c)

data Size =
  Size
    { width :: Int
    , height :: Int
    }

data Canvas =
  Canvas
    { begin :: Point
    , end :: Point
    , size :: Size
    , image :: RGBImage
    }

createCanvas :: Size -> Canvas
createCanvas s =
  Canvas
    (Point 0 0)
    (point (fromIntegral $ width s, fromIntegral $ height s))
    s
    (whiteCanvas s)

canvas :: (Double, Double) -> (Double, Double) -> Size -> Canvas
canvas (a, b) (c, d) s = Canvas (Point a b) (Point c d) s (whiteCanvas s)

homogenousCanvas :: Size -> Color -> Canvas
homogenousCanvas s clr = Canvas (Point (-1) (-1)) (Point 1 1) s blank
  where
    blank = generateImage renderer w h
      where
        renderer _ _ = rgb $ clr
        w = width s
        h = height s

cartesianCanvas :: Size -> Color -> Canvas
cartesianCanvas s clr = Canvas (Point (-1) 1) (Point 1 (-1)) s blank
  where
    blank = generateImage renderer w h
      where
        renderer _ _ = rgb $ clr
        w = width s
        h = height s

whiteCanvas :: Size -> RGBImage
whiteCanvas s = generateImage renderer w h
  where
    renderer _ _ = PixelRGB8 255 255 255
    w = width s
    h = height s

colorOf :: RGBImage -> (Int, Int) -> Color
img `colorOf` (x, y) = Color r g b
  where
    PixelRGB8 r g b = pixelAt img x y

scaleCoordinate :: Fractional a => (a, a) -> (a, a) -> a -> a
scaleCoordinate (a, b) (c, d) v = c + (v - a) * (d - c) / (b - a)

scaleCoord :: Canvas -> (Int, Int) -> Point
scaleCoord c (px, py) = Point x' y'
  where
    x' =
      scaleCoordinate
        (0, fromIntegral $ width $ size c)
        (x $ begin c, x $ end c)
        (fromIntegral px)
    y' =
      scaleCoordinate
        (0, fromIntegral $ height $ size c)
        (y $ begin c, y $ end c)
        (fromIntegral py)

draw :: Canvas -> Shape -> Color -> Canvas
draw c s clr = c {image = newImage}
  where
    img = image c
    w = width $ size c
    h = height $ size c
    newImage = generateImage renderer w h
    renderer x y = color
      where
        p = scaleCoord c (x, y)
        color =
          rgb $
          if p `inside` s
            then clr
            else img `colorOf` (x, y)

render :: Canvas -> String -> IO ()
render (Canvas {image = img}) path = writePng path img

domainMap :: Fractional n => n -> n -> n -> n -> n -> n
domainMap i min0 max0 min1 max1 = x
  where
    ratio = (i - min0) / (max0 - min0)
    x = min1 + (ratio * (max1 - min1))

d2px :: Double -> Pixel8
d2px d = (fromIntegral (round d :: Int)) :: Pixel8

degToRGB :: Double -> Color
degToRGB deg =
  let (RGB r g b) = hsl deg 1.0 0.5
   in Color (d2px $ 255 * r) (d2px $ 255 * g) (d2px $ 255 * b)
