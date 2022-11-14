module Shape where

import Codec.Picture
import Data.Array
import Data.Fixed (mod')
import Layout
import Matrix

data Shape
  = Polygon
      { vertices :: Points
      , center :: Point
      , boundingRect :: Bounds
      }
  | Ellipse
      { center :: Point
      , radiusX :: Double
      , radiusY :: Double
      , boundingRect :: Bounds
      -- , rotation :: Double
      }
  | Empty

getPolyBounds :: (Point, Point) -> [Point] -> (Point, Point)
getPolyBounds x [] = x
getPolyBounds (minP, maxP) (p:ps) = getPolyBounds (minP', maxP') ps
  where
    minP' = Point (min (x p) (x minP)) (min (y p) (y minP))
    maxP' = Point (max (x p) (x maxP)) (max (y p) (y maxP))

getMaximaAndMinima :: (Point, Point) -> Points -> Int -> Int -> (Point, Point)
getMaximaAndMinima (minP, maxP) ps n i =
  if i >= n - 1
    then (minP, maxP)
    else evalMinMax (minP, maxP) ps n i

evalMinMax (minP, maxP) ps n i =
  let p = ps ! i
      minP' = Point (min (x p) (x minP)) (min (y p) (y minP))
      maxP' = Point (max (x p) (x maxP)) (max (y p) (y maxP))
   in getMaximaAndMinima (minP', maxP') ps n (i + 1)

getBoundingRect :: Points -> Bounds
getBoundingRect ps = Bounds p1 p2
  where
    (p:ps') = elems ps
    (p1, p2) = getPolyBounds (p, p) ps'

getPolyCenter :: [Point] -> Int -> Point -> Point
getPolyCenter [] n p =
  Point ((x p) / (fromIntegral n)) ((y p) / (fromIntegral n))
getPolyCenter ((Point {x = x', y = y'}):ps) n p =
  getPolyCenter ps n (Point (x p + x') (y p + y'))

polyCenter :: Points -> Point
polyCenter ps = getPolyCenter (elems ps) (length ps) (Point 0 0)

ellipseDist :: Point -> Point -> Double -> Double -> Double
ellipseDist p c rx ry = xd + yd
  where
    xh = (x p) - (x c)
    xh2 = xh * xh
    yk = (y p) - (y c)
    yk2 = yk * yk
    xd = xh2 / (rx * rx)
    yd = yk2 / (ry * ry)

inBoundingRect :: Point -> Bounds -> Bool
inBoundingRect p b = px >= minX && py >= minY && px <= maxX && py <= maxY
  where
    minX = x $ minBP b
    minY = y $ minBP b
    maxX = x $ maxBP b
    maxY = y $ maxBP b
    px = x p
    py = y p

insidePolygon :: Point -> Points -> Bool
insidePolygon p polygon =
  insidePoly p polygon (indices polygon) (snd (bounds polygon)) False

insidePoly :: Point -> Points -> [Int] -> Int -> Bool -> Bool
insidePoly _ _ [] _ b = b
insidePoly p ps (i:is) j b = insidePoly p ps is i b'
  where
    p1 = ps ! i
    p2 = ps ! j
    xv = (((x p2) - (x p1)) * ((y p) - (y p1))) / ((y p2) - (y p1))
    cond1 = ((y p1) > (y p)) /= ((y p2) > (y p))
    cond2 = (x p) < xv + (x p1)
    b' =
      if cond1 && cond2
        then not b
        else b

insideEllipse :: Point -> Point -> Double -> Double -> Bool
insideEllipse p c a b =
  (((x p - x c) ** 2) / (a ** 2)) + (((y p - y c) ** 2) / (b ** 2)) <= 1

-- insideRotatedEllipse :: Point -> Point -> Double -> Double -> Double -> Bool
-- insideRotatedEllipse p c a b theta = xx + xy + yy <= a2 * b2
--   where
--     x' = x p - x c
--     y' = y p - y c
--     sin1 = sin theta
--     cos1 = cos theta
--     cos2 = (1 + (cos $ 2 * theta)) / 2
--     sin2 = 1 - cos2
--     a2 = a * a
--     b2 = b * b
--     x2 = x' * x'
--     y2 = y' * y'
--     xx = x2 * (a2 * sin2 + b2 * cos2)
--     xy = 2 * (b2 - a2) * sin1 * cos1 * x' * y'
--     yy = y2 * (a2 * cos2 + b2 * sin2)
inside :: Point -> Shape -> Bool
p `inside` (Polygon {vertices = ps, boundingRect = br}) =
  if not $ inBoundingRect p br
    then False
    else insidePolygon p ps
p `inside` (Ellipse { center = c
                    , radiusX = rx
                    , radiusY = ry
                    -- , rotation = a
                    , boundingRect = br
                    }) = insideEllipse p c rx ry

-- Shape Constructors
poly :: [(Double, Double)] -> Shape
poly ps = Polygon {vertices = points, center = c, boundingRect = b}
  where
    points = toPointArray ps
    b = getBoundingRect points
    c = polyCenter points

circle :: (Double, Double) -> Double -> Shape
circle (px, py) r =
  Ellipse {center = c, radiusX = r, radiusY = r, boundingRect = b} {-, rotation = 0-}
  where
    c = point (px, py)
    b = Bounds (point (px - r, py - r)) (point (px + r, py + r))

ellipse :: (Double, Double) -> Double -> Double -> Shape
ellipse c w h = Ellipse p w h b
  where
    p = point c
    minB = Point ((x p) - w) ((y p) - h)
    maxB = Point ((x p) + w) ((y p) + h)
    b = Bounds minB maxB

triangle :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Shape
triangle p1 p2 p3 = Polygon {vertices = ps, center = c, boundingRect = b}
  where
    ps = toPointArray ([p1, p2, p3])
    c = polyCenter ps
    b = getBoundingRect ps

square :: (Double, Double) -> Double -> Shape
square (px, py) s = Polygon {vertices = ps, center = c, boundingRect = b}
  where
    tl = (px, py)
    tr = (px + s, py)
    br = (px + s, py + s)
    bl = (px, py + s)
    ps = toPointArray [tl, tr, br, bl]
    b = Bounds (point tl) (point br)
    c = point (px + s / 2.0, py + s / 2.0)

csquare :: (Double, Double) -> Double -> Shape
csquare (px, py) s = square (px - s / 2, py - s / 2) s

rectangle :: (Double, Double) -> Double -> Double -> Shape
rectangle (px, py) w h = Polygon {vertices = ps, center = c, boundingRect = b}
  where
    tl = (px, py)
    tr = (px + w, py)
    br = (px + w, py + h)
    bl = (px, py + h)
    ps = toPointArray [tl, tr, br, bl]
    c = point (px + w / 2.0, py + h / 2.0)
    b = Bounds (point tl) (point br)

crectangle :: (Double, Double) -> Double -> Double -> Shape
crectangle (px, py) w h = rectangle (px - w / 2, py - h / 2) w h

rad :: Double -> Double
rad d = d * pi / 180

(.-) :: Double -> Double -> Double
a .- b = (a - b + 360) `mod'` 360

asPair :: n -> (n, n)
asPair n = (n, n)

degPairOnPoint :: (Double, Double) -> Double -> Double -> (Double, Double)
degPairOnPoint (cx, cy) d r = (cx + r * (cos $ rad d), cy + r * (sin $ rad d))

interweave :: [a] -> [a] -> [a]
interweave [] a = a
interweave a [] = a
interweave (a:as) (b:bs) = a : b : (interweave as bs)

star :: (Double, Double) -> Int -> Double -> Double -> Shape
star (cx, cy) n inner outer =
  Polygon {vertices = ps, center = c, boundingRect = b}
  where
    deg = 360 / (fromIntegral n)
    deg' = 270 - (deg / 2)
    outerPs =
      [ degPairOnPoint (cx, cy) (270 .- (d * deg)) outer
      | d <- [1 .. (fromIntegral n)]
      ]
    innerPs =
      [ degPairOnPoint (cx, cy) (deg' .- (d * deg)) inner
      | d <- [1 .. (fromIntegral n)]
      ]
    ps = toPointArray (interweave outerPs innerPs)
    b = getBoundingRect ps
    c = polyCenter ps

pcircle :: (Double, Double) -> Double -> Shape
pcircle (cx, cy) r =
  Polygon {vertices = ps, center = point (cx, cy), boundingRect = br}
  where
    n = 180 :: Double
    deg = 360 / (n - 1)
    ps =
      toPointArray
        [ ( cx + r * (cos $ rad (270 .- (deg * i)))
          , cy + r * (sin $ rad (270 .- (deg * i))))
        | i <- [0 .. n]
        ]
    br = getBoundingRect ps

-- aroundOrigin :: (Point -> Point) -> Point -> Points -> Points
-- aroundOrigin f c ps = ps'
--   where
--     origin = fmap (\p' -> Point (x p' - x c) (y p' - y c)) ps
--     ps'' = fmap f origin
--     ps' = fmap (\p' -> Point (x p' + x c) (y p' + y c)) ps''
-- rotate :: Double -> Shape -> Shape
-- rotate d (Polygon {vertices = ps, boundingRect = b, center = c}) =
--   Polygon {vertices = ps', boundingRect = b', center = c}
--   where
--     mat = Matrix (cos $ rad d) (-1 * (sin $ rad d)) (sin $ rad d) (cos $ rad d)
--     ps' = aroundOrigin (transform (Rotate mat)) c ps
--     b' = getBoundingRect ps'
-- rotate d e = e {rotation = rad d, boundingRect = b'}
--   where
--     b = boundingRect e
--     mat = Matrix (cos $ rad d) (-1 * (sin $ rad d)) (sin $ rad d) (cos $ rad d)
--     c = center e
--     b'' =
--       aroundOrigin (transform (Rotate mat)) c $
--       toPointArray [(lP $ minBP b), (lP $ maxBP b)]
--     b' = Bounds (b'' ! 0) (b'' ! 1)
-- translatePoint :: (Double, Double) -> Point -> Point
-- translatePoint (tx, ty) (Point x' y') = Point (x' + tx) (y' + ty)
-- translate :: (Double, Double) -> Shape -> Shape
-- translate t (Ellipse { center = c
--                      , radiusX = rx
--                      , radiusY = ry
--                      , boundingRect = b
--                      , rotation = r
--                      }) = e
--   where
--     e =
--       Ellipse
--         { center = c'
--         , radiusX = rx
--         , radiusY = ry
--         , boundingRect = br
--         , rotation = r
--         }
--     c' = translatePoint t $ c
--     br = Bounds (translatePoint t $ minBP b) (translatePoint t $ maxBP b)
-- translate t p = p {vertices = vs, center = c, boundingRect = br}
--   where
--     vs = fmap (translatePoint t) (vertices p)
--     c = translatePoint t $ center p
--     br =
--       Bounds
--         (translatePoint t $ minBP $ boundingRect p)
--         (translatePoint t $ maxBP $ boundingRect p)
-- scalePoint :: (Double, Double) -> Point -> Point
-- scalePoint (sx, sy) (Point a b) = Point (a * sx) (b * sy)
-- scale :: (Double, Double) -> Shape -> Shape
-- scale s (Ellipse { center = c
--                  , radiusX = rx
--                  , radiusY = ry
--                  , boundingRect = b
--                  , rotation = r
--                  }) = e
--   where
--     (sx, sy) = s
--     e =
--       Ellipse
--         { center = c
--         , radiusX = rx * sx
--         , radiusY = ry * sy
--         , boundingRect = br
--         , rotation = r
--         }
--     b' =
--       aroundOrigin (scalePoint s) c $
--       toPointArray [(lP $ minBP b), (lP $ maxBP b)]
--     br = Bounds (b' ! 0) (b' ! 1)
-- scale s p = p {vertices = ps, boundingRect = br}
--   where
--     c = center p
--     (sx, sy) = s
--     ps = aroundOrigin (scalePoint s) c $ vertices p
--     b = aroundOrigin (scalePoint s) c $ boundsAsPointsList $ boundingRect p
--     br = Bounds (b ! 0) (b ! 1)
-- scaleX, scaleY :: Double -> Shape -> Shape
-- scaleX sx s = scale (sx, 1) s
-- scaleY sy s = scale (1, sy) s
-- translateX, translateY :: Double -> Shape -> Shape
-- translateX tx s = translate (tx, 0) s
-- translateY ty s = translate (0, ty) s
-- shearPoint :: Matrix -> Point -> Point
-- shearPoint m p = m `multiply` p
-- shear :: (Double, Double) -> Shape -> Shape
-- shear (sx, sy) (Polygon {vertices = ps, boundingRect = b, center = c}) =
--   (Polygon {vertices = ps', boundingRect = b', center = c'})
--   where
--     mat = Matrix 1 sx sy 1
--     ps' = aroundOrigin (shearPoint mat) c ps
--     b' = getBoundingRect ps'
--     c' = polyCenter ps'
-- shear (sx, sy) e = e {radiusX = dX, radiusY = dY, rotation = rD}
--   where
--     d = rotation e
--     bs = boundedCorners $ boundingRect e
--     mat = Matrix (cos $ d) (-1 * (sin $ d)) (sin $ d) (cos $ d)
--     c = center e
--     bs' = aroundOrigin (transform (Rotate mat)) c bs
--     mat' = Matrix 1 sx sy 1
--     bs'' = aroundOrigin (shearPoint mat') c bs'
--     mpL = midPoint (bs'' ! 0) (bs'' ! 3)
--     mpT = midPoint (bs'' ! 0) (bs'' ! 1)
--     mpR = midPoint (bs'' ! 1) (bs'' ! 2)
--     mpB = midPoint (bs'' ! 2) (bs'' ! 3)
--     dX = (pointDistance mpL mpR) / 2
--     dY = (pointDistance mpT mpB) / 2
--     mLR = slope mpL mpR
--     rD = atan mLR
-- -- shear (sx, sy) (Ellipse { center = c, radiusX = rx, radiusY = ry, rotation = r }) =
-- shearX, shearY :: Double -> Shape -> Shape
-- shearX sx s = shear (sx, 0) s
-- shearY sy s = shear (0, sy) s
translate :: (Double, Double) -> Mat3
translate = mtranslate

rotate :: Double -> Mat3
rotate = mrotate

scale :: (Double, Double) -> Mat3
scale = mscale

shear :: (Double, Double) -> Mat3
shear = mshear

slope :: Point -> Point -> Double
slope p1 p2 = (y p2 - y p1) / (x p2 - x p1)

degr :: Double -> Double
degr r = r * 180 / pi

translateX, translateY, shearX, shearY, scaleX, scaleY :: Double -> Mat3
translateX t = translate (t, 0)

translateY t = translate (0, t)

scaleX s = translate (s, 1)

scaleY s = translate (1, s)

shearX s = shear (s, 0)

shearY s = shear (0, s)

pointDistance :: Point -> Point -> Double
pointDistance p1 p2 = sqrt ((x p2 - x p1) ** 2 + (y p2 - y p1) ** 2)

-- ellipseCoords :: Shape -> ((Point, Point, Double), (Point, Point, Double))
-- ellipseCoords (Ellipse {center = c, radiusX = rx, radiusY = ry, rotation = d}) =
--   ((x', x'', ax), (x'', x''', ax'))
--   where
--     x' = translatePoint (rx, 0) c
--     y' = translatePoint (0, ry) c
--     mat = Matrix (cos $ d) (-1 * (sin $ d)) (sin $ d) (cos $ d)
--     ps' = aroundOrigin (transform (Rotate mat)) c $ toPointArray [lP x', lP y']
--     (x'', y'') = (ps' ! 0, ps' ! 1)
--     (mx, my) = (slope x'' c, slope y'' c)
--     (ax) = (degr $ atan mx)
--     mat' = Matrix 1 0.5 0 1
--     ps'' = aroundOrigin (shearPoint mat') c ps'
--     (x''', y''') = (ps'' ! 0, ps'' ! 1)
--     (mx', my') = (slope x''' c, slope y''' c)
--     (ax') = (degr $ atan mx')
applyTransform :: Shape -> Mat3 -> Shape
applyTransform (Ellipse { center = c
                        , radiusX = rx
                        , radiusY = ry
                        -- , rotation = r
                        , boundingRect = b
                        }) _ =
  Ellipse
    { center = c
    , radiusX = rx
    , radiusY = ry
    , boundingRect = b {-rotation = r,-}
    }
applyTransform s m = s {vertices = ps, center = c, boundingRect = br}
  where
    ps = fmap (m `matMul`) $ vertices s
    c = polyCenter ps
    br = getBoundingRect ps
