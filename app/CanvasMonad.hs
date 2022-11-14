module CanvasMonad where

import Canvas
import Codec.Picture
import Data.Array
import Layout
import Shape

import Control.Monad (ap, liftM)
import Matrix

type Transform = [Mat3]

data Draws s =
  Draws
    { draws :: [(Shape, Transform, Color)]
    , state :: s
    }

instance Functor Draws where
  fmap = liftM

instance Applicative Draws where
  pure = Draws []
  (<*>) = ap

instance Monad Draws where
  return = pure
  m >>= f =
    let Draws d s = m
        Draws d' s' = f s
     in Draws (d ++ d') s'

drawS :: Shape -> Transform -> Color -> Draws ()
drawS s c t = Draws [(s, c, t)] ()

drawAll :: [(Shape, Transform, Color)] -> Draws ()
drawAll sc = Draws sc ()

getDraws :: Draws a -> [(Shape, Transform, Color)]
getDraws (Draws {draws = d}) = d

applyDraws :: [(Shape, Transform, Color)] -> Canvas -> Canvas
applyDraws [] c = c
applyDraws ((shape, trans, color):d) c = applyDraws d c'
  where
    c' = draw c (applyTransform shape $ mtransform trans) color

f `onto` c = f c

finishDrawing :: Monad m => m ()
finishDrawing = return ()

paint :: Draws a -> Canvas -> Canvas
paint (Draws {draws = d}) c = applyDraws d c

type Drawing = Draws ()

getFirstColor :: [(Shape, Color)] -> Point -> (Bool, Shape, Color)
getFirstColor [] p = (False, Empty, Color 0 0 0)
getFirstColor ((shp, clr):as) p
  | p `inside` shp = (True, shp, clr)
  | otherwise = getFirstColor as p

optDraw :: [(Shape, Transform, Color)] -> Canvas -> Canvas
optDraw shc c = c {image = newImage}
  where
    shapes =
      fmap (\(sh, trs, clr) -> (applyTransform sh $ mtransform trs, clr)) $
      reverse shc
    img = image c
    w = width $ size c
    h = height $ size c
    newImage = generateImage renderer w h
    renderer x y = color
      where
        p = scaleCoord c (x, y)
        (found, shp, clr) = getFirstColor shapes p
        color =
          rgb $
          if found
            then colorValue clr shp p
            else img `colorOf` (x, y)

shape :: Shape -> Transform -> Draws ()
shape s t = Draws [(s, t, Color 255 255 255)] ()

type MaskImage = Drawing

optPaint :: Drawing -> Canvas -> Canvas
optPaint (Draws {draws = d}) c = optDraw d c

maskDraw :: [(Shape, Transform, Color)] -> Canvas -> Canvas
maskDraw shc c = c {image = newImage}
  where
    shapes =
      fmap (\(sh, trs, clr) -> (applyTransform sh $ mtransform trs, clr)) $
      reverse shc
    img = image c
    w = width $ size c
    h = height $ size c
    newImage = generateImage renderer w h
    renderer x y = color
      where
        p = scaleCoord c (x, y)
        (found, _, _) = getFirstColor shapes p
        color =
          rgb $
          if found
            then Color 255 255 255
            else Color 0 0 0

data Mask
  = AND
  | OR
  deriving (Show, Eq)

andMask :: Canvas -> Canvas -> Canvas
m `andMask` c = c {image = newImage}
  where
    w = width $ size m
    h = height $ size m
    maskImage = image m
    currentImage = image c
    newImage = generateImage renderer w h
    renderer x y = color
      where
        mc = maskImage `colorOf` (x, y)
        cc = currentImage `colorOf` (x, y)
        color = rgb $ colorAnd mc cc

orMask :: Canvas -> Canvas -> Canvas
m `orMask` c = c {image = newImage}
  where
    w = width $ size m
    h = height $ size m
    maskImage = image m
    currentImage = image c
    newImage = generateImage renderer w h
    renderer x y = color
      where
        mc = maskImage `colorOf` (x, y)
        cc = currentImage `colorOf` (x, y)
        color = rgb $ colorOr mc cc

makeMask :: MaskImage -> Size -> Canvas
makeMask drws s = maskCanvas
  where
    d = draws drws
    cvs = homogenousCanvas s $ Color 0 0 0
    maskCanvas = maskDraw d cvs

makeInverseMask :: MaskImage -> Size -> Canvas
makeInverseMask drws s = maskCanvas
  where
    d = draws drws
    cvs = homogenousCanvas s $ Color 0 0 0
    maskCanvas = maskInvDraw d cvs

maskInvDraw :: [(Shape, Transform, Color)] -> Canvas -> Canvas
maskInvDraw shc c = c {image = newImage}
  where
    shapes =
      fmap (\(sh, trs, clr) -> (applyTransform sh $ mtransform trs, clr)) $
      reverse shc
    img = image c
    w = width $ size c
    h = height $ size c
    newImage = generateImage renderer w h
    renderer x y = color
      where
        p = scaleCoord c (x, y)
        (found, _, _) = getFirstColor shapes p
        color =
          rgb $
          if found
            then Color 0 0 0
            else Color 255 255 255

asAndMaskOn :: MaskImage -> Canvas -> Canvas
mi `asAndMaskOn` c = c'
  where
    d = draws mi
    masked = makeMask mi s
    s = size c
    mc = maskDraw d $ homogenousCanvas s $ Color 0 0 0
    c' = mc `andMask` c

asOrMaskOn :: MaskImage -> Canvas -> Canvas
mi `asOrMaskOn` c = c'
  where
    d = draws mi
    masked = makeMask mi s
    s = size c
    mc = maskDraw d $ homogenousCanvas s $ Color 0 0 0
    c' = mc `orMask` c
