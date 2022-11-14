module Drawing
  ( CM.Drawing
  , draw
  , CM.drawAll
  , paint
  , CM.onto
  , S.Shape(..)
  , CR.Size(..)
  , L.Point(..)
  , L.Bounds(..)
  , L.midPoint
  , L.boundedCorners
  , L.getBoundingCorners
  , S.circle
  , S.square
  , S.csquare
  , S.rectangle
  , S.star
  , S.pcircle
  , polygon
  , S.ellipse
  , S.triangle
  , S.translate
  , S.translateX
  , S.translateY
  , S.scale
  , S.scaleX
  , S.scaleY
  , S.rotate
  , S.shear
  , S.shearX
  , S.shearY
  , CR.Color(..)
  , absoluteCanvas
  , CR.canvas
  , CR.Canvas(..)
  , CR.render
  , P.Pixel8(..)
  , (.-)
  , S.pointDistance
  , CM.Mask(..)
  , CM.makeMask
  , CM.MaskImage
  , CM.shape
  , CM.orMask
  , CM.andMask
  , CR.homogenousCanvas
  , CR.cartesianCanvas
  , CM.finishDrawing
  , S.crectangle
  , CM.asAndMaskOn
  , CM.asOrMaskOn
  , CM.Transform
  , CM.makeInverseMask
  , CR.domainMap
  , CR.d2px
  , CR.degToRGB
  , S.rad
  ) where

import qualified Canvas as CR
import qualified CanvasMonad as CM
import qualified Codec.Picture as P
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Data.Fixed (mod')
import qualified Layout as L
import qualified Shape as S
import Shape ((.-))

absoluteCanvas = CR.createCanvas

polygon = S.poly

draw = CM.drawS

paint = CM.optPaint
