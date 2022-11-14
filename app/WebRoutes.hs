{-# LANGUAGE OverloadedStrings #-}

module WebRoutes
  ( runPage
  ) where

import Data.Foldable (for_)
import Data.Text.Lazy
import Network.Wai.Middleware.Static
import System.IO
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty

runPage :: IO ()
runPage = do
  scotty 3000 $ do
    middleware $ staticPolicy (addBase "assets")
    get "/" $ do
      html $
        R.renderHtml $ do
          H.docType
          H.html $ do
            H.head $ do
              H.link H.! A.rel "stylesheet" H.! A.href "./styles.css"
              H.link H.! A.rel "shortcut icon" H.! A.href "./rainbowStars.png" H.!
                A.type_ "image/x-icon"
              H.title "Image DSL"
            H.body $ do
              H.h1 "Image Drawing DSL"
              for_ images $ \image -> generatedImage image

generatedImage :: (Text, [H.AttributeValue], [Text]) -> H.Html
generatedImage (imgTitle, imgs, codeText) = do
  H.div H.! A.class_ "image-container" $ do
    H.h2 $ H.toHtml imgTitle
    H.div H.! A.class_ "images" $ do
      for_ imgs (\filename -> H.img H.! A.src filename)
    H.pre $ H.toHtml $ intercalate "\n" codeText

images :: [(Text, [H.AttributeValue], [Text])]
images =
  [ rainbowStars
  , connectedSquares
  , haskellLogo
  , radialGradient
  , combinedImages
  , advancedMasking
  ]

rainbowStars :: (Text, [H.AttributeValue], [Text])
rainbowStars =
  ( "Rainbow Stars"
  , ["./rainbowStars.png"]
  , [ "import Drawing"
    , ""
    , "rainbowStars :: Int -> Int -> [(Shape, Transform, Color)]"
    , "rainbowStars _ 0 = []"
    , "rainbowStars n i ="
    , "  (star (0, 0) 5 innerRad outerRad, [rotate deg], clr) : rainbowStars n (i - 1)"
    , "  where"
    , "    innerRad = (i' / 100.0)"
    , "    outerRad = (i' / 50.0)"
    , "    deg = (i' * 0.75)"
    , "    i' = (fromIntegral i)"
    , "    clr = degToRGB $ domainMap i' 0 (fromIntegral n) 0 360"
    , ""
    , "rainbowStarDrawing :: Drawing"
    , "rainbowStarDrawing = do"
    , "  drawAll $ rainbowStars 128 128"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "  let drawing = paint rainbowStarDrawing `onto` blankCanvas"
    , "  render drawing \"rainbowStars.png\""
    ])

connectedSquares :: (Text, [H.AttributeValue], [Text])
connectedSquares =
  ( "Connected Squares"
  , ["./connectedSquares.png"]
  , [ "import Drawing"
    , ""
    , "connectedSquares :: Drawing"
    , "connectedSquares"
    , " = do"
    , "    -- Corner squares"
    , "  draw (csquare (0.5, 0.5) 0.5) [] $ Color 255 0 0"
    , "  draw (csquare (0.5, -0.5) 0.5) [] $ Color 255 200 0"
    , "  draw (csquare (-0.5, -0.5) 0.5) [] $ Color 50 180 0"
    , "  draw (csquare (-0.5, 0.5) 0.5) [] $ Color 0 150 255"
    , "    -- White Cut-outs"
    , "  draw (csquare (0.5, 0.5) 0.25) [] $ Color 255 255 255"
    , "  draw (csquare (0.5, -0.5) 0.25) [] $ Color 255 255 255"
    , "  draw (csquare (-0.5, -0.5) 0.25) [] $ Color 255 255 255"
    , "  draw (csquare (-0.5, 0.5) 0.25) [] $ Color 255 255 255"
    , "    -- Black joints"
    , "  draw (crectangle (0, 0.5) 0.5 0.125) [] $ Color 0 0 0"
    , "  draw (crectangle (0, -0.5) 0.5 0.125) [] $ Color 0 0 0"
    , "  draw (crectangle (0.5, 0) 0.125 0.5) [] $ Color 0 0 0"
    , "  draw (crectangle (-0.5, 0) 0.125 0.5) [] $ Color 0 0 0"
    , "    -- Center rhombus"
    , "  draw (csquare (0, 0) $ sqrt (2 * 0.5 ** 2)) [rotate 45] $ Color 160 0 255"
    , "  draw (csquare (0, 0) $ (sqrt (2 * 0.5 ** 2)) / (1.125 * sqrt 2)) [rotate 45] $ Color 255 255 255"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "  let drawing = paint connectedSquares `onto` blankCanvas"
    , "  render drawing \"connectedSquares.png\""
    ])

haskellLogo :: (Text, [H.AttributeValue], [Text])
haskellLogo =
  ( "Masking with the Haskell Logo"
  , ["./rainbowStars.png", "./haskellMask.png", "./haskellLogo.png"]
  , [ "import Drawing"
    , ""
    , "{- see above for rainbowStarDrawing -}"
    , ""
    , "haskellLogo :: MaskImage"
    , "haskellLogo = do"
    , "  shape"
    , "    (polygon"
    , "       [ (-0.5, -0.5)"
    , "       , (-0.25, -0.5)"
    , "       , (0.083333, 0)"
    , "       , (-0.25, 0.5)"
    , "       , (-0.5, 0.5)"
    , "       , (-0.16666, 0)"
    , "       ])"
    , "    [translateX (-0.25)]"
    , "  shape"
    , "    (polygon"
    , "       [ (-0.16666, -0.5)"
    , "       , (0.083333, -0.5)"
    , "       , (0.75, 0.5)"
    , "       , (0.5, 0.5)"
    , "       , (0.291666, 0.1875)"
    , "       , (0.083333, 0.5)"
    , "       , (-0.16666, 0.5)"
    , "       , (0.16666, 0)"
    , "       ])"
    , "    [translateX (-0.25)]"
    , "  shape"
    , "    (polygon"
    , "       [ (0.361111, -0.208333)"
    , "       , (0.916666, -0.208333)"
    , "       , (0.916666, -0.041666)"
    , "       , (0.472222, -0.041666)"
    , "       ])"
    , "    [translateX (-0.25)]"
    , "  shape"
    , "    (polygon"
    , "       [ (0.527777, 0.041666)"
    , "       , (0.916666, 0.041666)"
    , "       , (0.916666, 0.208333)"
    , "       , (0.638888, 0.208333)"
    , "       ])"
    , "    [translateX (-0.25)]"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "  let drawing = paint rainbowStarDrawing `onto` blankCanvas"
    , "      haskellMask = makeMask haskellLogo (size drawing)"
    , "      maskedLogo = haskellMask `andMask` drawing"
    , "  render drawing \"rainbowStars.png\""
    , "  render haskellMask \"haskellMask.png\""
    , "  render maskedLogo \"haskellLogo.png\""
    ])

radialGradient :: (Text, [H.AttributeValue], [Text])
radialGradient =
  ( "Radial Rainbow Gradient"
  , ["./radialGradient.png"]
  , [ "import Drawing"
    , ""
    , "rainbow :: Shape -> Point -> Color"
    , "rainbow s p ="
    , "  let (tl, tr, br, bl) = getBoundingCorners $ boundingRect s"
    , "      maxR ="
    , "        max"
    , "          (pointDistance (center s) (midPoint tl tr))"
    , "          (pointDistance (center s) (midPoint tl bl))"
    , "      d = pointDistance p (center s)"
    , "      res = domainMap d 0 maxR 0 360"
    , "      clr = degToRGB res"
    , "   in clr"
    , ""
    , "radialRainbowGradiant :: Drawing"
    , "radialRainbowGradiant = do"
    , "  draw (csquare (0, 0) 0.5) [scale (2, 2), rotate 45] $ ColorFn $ rainbow"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "  let drawing = paint radialRainbowGradient `onto` blankCanvas"
    , "  render drawing \"radialGradient.png\""
    ])

combinedImages :: (Text, [H.AttributeValue], [Text])
combinedImages =
  ( "Combining Drawings"
  , ["./rainbowStars.png", "./combinedImage.png"]
  , [ "import Drawing"
    , ""
    , "{- see above for rainbowStarDrawing -}"
    , ""
    , "starColouring :: Color"
    , "starColouring = ColorFn $"
    , "      (\\s p ->"
    , "         let clr = degToRGB $ (domainMap pDist 0 (maxDist) 60 30)"
    , "             pDist = pointDistance p (center s)"
    , "             (tl, tr, _, _) = getBoundingCorners $ boundingRect s"
    , "             maxDist = pointDistance (center s) (midPoint tl tr)"
    , "          in clr)"
    , ""
    , "combinedImage :: Drawing"
    , "combinedImage = do"
    , "  rainbowStarDrawing -- Existing drawing"
    , "  draw (star (0, 0) 5 0.135 0.35) [rotate (45), translate (-0.65, -0.65)] starColouring"
    , "  draw (star (0, 0) 5 0.135 0.35) [rotate (-45), translate (0.65, -0.65)] starColouring"
    , "  draw (star (0, 0) 5 0.135 0.35) [rotate (135), translate (-0.65, 0.65)] starColouring"
    , "  draw (star (0, 0) 5 0.135 0.35) [rotate (-135), translate (0.65, 0.65)] starColouring"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "  let drawing = paint combinedImage `onto` blankCanvas"
    , "  render drawing \"combinedImage.png\""
    ])

advancedMasking :: (Text, [H.AttributeValue], [Text])
advancedMasking =
  ( "Advanced Images with Masking"
  , [ "./waveCircle.png"
    , "./haskellMask.png"
    , "./waveLogo.png"
    , "./rainbowStars.png"
    , "./inverseMask.png"
    , "./background.png"
    , "./finalImage.png"
    ]
  , [ "import Drawing"
    , ""
    , "{- see above for rainbowStarDrawing and haskellLogo -}"
    , ""
    , "waveColouring :: Color"
    , "waveColouring ="
    , "  ColorFn $"
    , "  (\\_ p ->"
    , "     let c ="
    , "           d2px"
    , "             (domainMap"
    , "                (sin $ rad (360 * (1 + x p)) + (cos $ rad (1 + y p) * 360))"
    , "                (-1)"
    , "                1"
    , "                30"
    , "                200)"
    , "      in Color c c c)"
    , ""
    , "waveCircle :: Drawing"
    , "waveCircle = do"
    , "  draw (circle (0, 0) 1) [] waveColouring"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "  let rainbowStars = paint rainbowStarDrawing `onto` blankCanvas"
    , "      circleWaves = paint waveCircle `onto` blankCanvas"
    , "      logoMask = makeMask haskellLogo (size blankCanvas)"
    , "      inverseMask = makeInverseMask haskellLogo (size blankCanvas)"
    , "      waveLogo = logoMask `andMask` circleWaves"
    , "      background = inverseMask `andMask` rainbowStars"
    , "      finalImage = waveLogo `orMask` background"
    , "  render rainbowStars \"rainbowStars.png\""
    , "  render circleWaves \"waveCircle.png\""
    , "  render logoMask \"logoMask.png\""
    , "  render inverseMask \"inverseMask.png\""
    , "  render waveLogo \"waveLogo.png\""
    , "  render background \"background.png\""
    , "  render finalImage \"finalImage.png\""
    ])
