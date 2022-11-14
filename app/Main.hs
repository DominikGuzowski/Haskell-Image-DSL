module Main
  ( main
  ) where

import Drawing
import WebRoutes

rainbowStars :: Int -> Int -> [(Shape, Transform, Color)]
rainbowStars _ 0 = []
rainbowStars n i =
  (star (0, 0) 5 innerRad outerRad, [rotate deg], clr) : rainbowStars n (i - 1)
  where
    innerRad = (i' / 100.0)
    outerRad = (i' / 50.0)
    deg = (i' * 0.75)
    i' = (fromIntegral i)
    clr = degToRGB $ domainMap i' 0 (fromIntegral n) 0 360

rainbow :: Double -> Shape -> Point -> Color
rainbow factor s p =
  let (tl, tr, _, bl) = getBoundingCorners $ boundingRect s
      maxR =
        max
          (pointDistance (center s) (midPoint tl tr))
          (pointDistance (center s) (midPoint tl bl))
      d = pointDistance p (center s)
      res = domainMap d 0 (maxR * factor) 0 360
      clr = degToRGB res
   in clr

rainbowStarDrawing :: Drawing
rainbowStarDrawing = do
  drawAll $ rainbowStars 128 128

blankCanvas :: Canvas
blankCanvas = homogenousCanvas (Size 512 512) $ Color 255 255 255

connectedSquares :: Drawing
connectedSquares
    -- Corner squares
 = do
  draw (csquare (0.5, 0.5) 0.5) [] $ Color 255 0 0
  draw (csquare (0.5, -0.5) 0.5) [] $ Color 255 200 0
  draw (csquare (-0.5, -0.5) 0.5) [] $ Color 50 180 0
  draw (csquare (-0.5, 0.5) 0.5) [] $ Color 0 150 255
    -- White Cut-outs
  draw (csquare (0.5, 0.5) 0.25) [] $ Color 255 255 255
  draw (csquare (0.5, -0.5) 0.25) [] $ Color 255 255 255
  draw (csquare (-0.5, -0.5) 0.25) [] $ Color 255 255 255
  draw (csquare (-0.5, 0.5) 0.25) [] $ Color 255 255 255
    -- Black joints
  draw (crectangle (0, 0.5) 0.5 0.125) [] $ Color 0 0 0
  draw (crectangle (0, -0.5) 0.5 0.125) [] $ Color 0 0 0
  draw (crectangle (0.5, 0) 0.125 0.5) [] $ Color 0 0 0
  draw (crectangle (-0.5, 0) 0.125 0.5) [] $ Color 0 0 0
    -- Center rhombus
  draw (csquare (0, 0) $ sqrt (2 * 0.5 ** 2)) [rotate 45] $ Color 160 0 255
  draw (csquare (0, 0) $ (sqrt (2 * 0.5 ** 2)) / (1.125 * sqrt 2)) [rotate 45] $
    Color 255 255 255

haskellLogo :: MaskImage
haskellLogo = do
  shape
    (polygon
       [ (-0.5, -0.5)
       , (-0.25, -0.5)
       , (0.083333, 0)
       , (-0.25, 0.5)
       , (-0.5, 0.5)
       , (-0.16666, 0)
       ])
    [translateX (-0.25)]
  shape
    (polygon
       [ (-0.16666, -0.5)
       , (0.083333, -0.5)
       , (0.75, 0.5)
       , (0.5, 0.5)
       , (0.291666, 0.1875)
       , (0.083333, 0.5)
       , (-0.16666, 0.5)
       , (0.16666, 0)
       ])
    [translateX (-0.25)]
  shape
    (polygon
       [ (0.361111, -0.208333)
       , (0.916666, -0.208333)
       , (0.916666, -0.041666)
       , (0.472222, -0.041666)
       ])
    [translateX (-0.25)]
  shape
    (polygon
       [ (0.527777, 0.041666)
       , (0.916666, 0.041666)
       , (0.916666, 0.208333)
       , (0.638888, 0.208333)
       ])
    [translateX (-0.25)]

radialRainbowGradiant :: Drawing
radialRainbowGradiant = do
  draw (csquare (0, 0) 0.5) [scale (2, 2), rotate 45] $ ColorFn $ rainbow 1

starColouring :: Color
starColouring =
  ColorFn $
  (\s p ->
     let clr = degToRGB $ (domainMap pDist 0 (maxDist) 60 30)
         pDist = pointDistance p (center s)
         (tl, tr, _, _) = getBoundingCorners $ boundingRect s
         maxDist = pointDistance (center s) (midPoint tl tr)
      in clr)

waveColouring :: Color
waveColouring =
  ColorFn $
  (\_ p ->
     let c =
           d2px
             (domainMap
                (sin $ rad (360 * (1 + x p)) + (cos $ rad (1 + y p) * 360))
                (-1)
                1
                30
                200)
      in Color c c c)

combinedImage :: Drawing
combinedImage = do
  rainbowStarDrawing
  draw
    (star (0, 0) 5 0.135 0.35)
    [rotate (45), translate (-0.65, -0.65)]
    starColouring
  draw
    (star (0, 0) 5 0.135 0.35)
    [rotate (-45), translate (0.65, -0.65)]
    starColouring
  draw
    (star (0, 0) 5 0.135 0.35)
    [rotate (135), translate (-0.65, 0.65)]
    starColouring
  draw
    (star (0, 0) 5 0.135 0.35)
    [rotate (-135), translate (0.65, 0.65)]
    starColouring

waveCircle :: Drawing
waveCircle = do
  draw (circle (0, 0) 1) [] waveColouring

sq1 :: Drawing
sq1 = do
  draw (csquare (0, 0) 1) [rotate 45, translate (0.5, 0)] $ Color 255 0 0

sq2 :: Drawing
sq2 = do
  draw (csquare (0.5, 0) 1) [translate (-0.5, 0), rotate 45, translate (0.5, 0)] $
    Color 0 255 0

sq3 :: Drawing
sq3 = do
  draw (csquare (0.5, 0) 1) [rotate 45] $ Color 0 0 255

main :: IO ()
main = do
  let rainbowStarPainting = paint rainbowStarDrawing `onto` blankCanvas
      waveCirclePainting = paint waveCircle `onto` blankCanvas
      haskellLogoMask = makeMask haskellLogo (size blankCanvas)
      haskellLogoMaskInv = makeInverseMask haskellLogo (size blankCanvas)
      waveLogo = haskellLogoMask `andMask` waveCirclePainting
      background = haskellLogoMaskInv `andMask` rainbowStarPainting
      finalImage = waveLogo `orMask` background
      sq1i = paint sq1 `onto` blankCanvas
      sq2i = paint sq2 `onto` blankCanvas
      sq3i = paint sq3 `onto` blankCanvas
      combinedI = paint combinedImage `onto` blankCanvas
      squares = paint connectedSquares `onto` blankCanvas
      rainbowGrad = paint radialRainbowGradiant `onto` blankCanvas
  render waveCirclePainting "assets/waveCircle.png"
  render haskellLogoMaskInv "assets/inverseMask.png"
  render combinedI "assets/combinedImage.png"
  render waveLogo "assets/waveLogo.png"
  render background "assets/background.png"
  render finalImage "assets/finalImage.png"
  render squares "assets/connectedSquares.png"
  render rainbowGrad "assets/radialGradient.png"
  render sq1i "sq1.png"
  render sq2i "sq2.png"
  render sq3i "sq3.png"
  runPage
