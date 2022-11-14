module Matrix where

import Layout (Point(..), Vector)

data Mat3 =
  Mat3
    { cells :: (Mat3Triple, Mat3Triple, Mat3Triple)
    }
  deriving (Show, Eq)

type Mat3Triple = (Double, Double, Double)

mat3 :: Mat3Triple -> Mat3Triple -> Mat3Triple -> Mat3
mat3 a b c = Mat3 (a, b, c)

mcol :: Int -> Mat3 -> Mat3Triple
mcol 1 (Mat3 ((a, _, _), (b, _, _), (c, _, _))) = (a, b, c)
mcol 2 (Mat3 ((_, a, _), (_, b, _), (_, c, _))) = (a, b, c)
mcol 3 (Mat3 ((_, _, a), (_, _, b), (_, _, c))) = (a, b, c)
mcol _ _ = error "Mat3 out of bounds"

mcol' :: Int -> Mat3Triple -> Double
mcol' 1 (a, _, _) = a
mcol' 2 (_, a, _) = a
mcol' 3 (_, _, a) = a
mcol' _ _ = error "Mat3 out of bounds"

mrow :: Int -> Mat3 -> Mat3Triple
mrow 1 (Mat3 (a, _, _)) = a
mrow 2 (Mat3 (_, a, _)) = a
mrow 3 (Mat3 (_, _, a)) = a
mrow _ _ = error "Mat3 out of bounds"

mget :: Mat3 -> Int -> Int -> Double
mget m i j = mcol' j $ mrow i m

(.*.) :: Mat3Triple -> Mat3Triple -> Double
(a, b, c) .*. (a', b', c') = a * a' + b * b' + c * c'

(*.*) :: Mat3 -> Mat3 -> Mat3
m1 *.* m2 = mat3 r1 r2 r3
  where
    r1 =
      ( mrow 1 m1 .*. mcol 1 m2
      , mrow 1 m1 .*. mcol 2 m2
      , mrow 1 m1 .*. mcol 3 m2)
    r2 =
      ( mrow 2 m1 .*. mcol 1 m2
      , mrow 2 m1 .*. mcol 2 m2
      , mrow 2 m1 .*. mcol 3 m2)
    r3 =
      ( mrow 3 m1 .*. mcol 1 m2
      , mrow 3 m1 .*. mcol 2 m2
      , mrow 3 m1 .*. mcol 3 m2)

(./.) :: Mat3 -> Double -> Mat3
(Mat3 ((a, b, c), (d, e, f), (g, h, i))) ./. k =
  mat3 (a / k, b / k, c / k) (d / k, e / k, f / k) (g / k, h / k, i / k)

mat3id :: Mat3
mat3id = mat3 (1, 0, 0) (0, 1, 0) (0, 0, 1)

matMul :: Mat3 -> Vector -> Vector
matMul m (Point a b) = Point a' b'
  where
    p = (a, b, 1)
    a' = mrow 1 m .*. p
    b' = mrow 2 m .*. p

mat3inv :: Mat3 -> Mat3
mat3inv m = mat3adj m ./. mat3det m

mat3det :: Mat3 -> Double
mat3det (Mat3 ((a, b, c), (d, e, f), (g, h, i))) = det
  where
    adet = e * i - f * h
    bdet = d * i - f * g
    cdet = d * h - e * g
    det = (a * adet) - (b * bdet) + (c * cdet)

mat3adj :: Mat3 -> Mat3
mat3adj m = mat3trans $ mat3cof m

mat2det :: (Double, Double) -> (Double, Double) -> Double
mat2det (a, b) (c, d) = a * d - b * c

mat3cof :: Mat3 -> Mat3
mat3cof (Mat3 ((a, b, c), (d, e, f), (g, h, i))) =
  mat3 (c1, -c2, c3) (-c4, c5, -c6) (c7, -c8, c9)
  where
    c1 = mat2det (e, f) (h, i)
    c2 = mat2det (d, f) (g, i)
    c3 = mat2det (d, e) (g, h)
    c4 = mat2det (b, c) (h, i)
    c5 = mat2det (a, c) (g, i)
    c6 = mat2det (a, b) (g, h)
    c7 = mat2det (b, c) (e, f)
    c8 = mat2det (a, c) (d, f)
    c9 = mat2det (a, b) (d, e)

mat3trans :: Mat3 -> Mat3
mat3trans m = mat3 (mcol 1 m) (mcol 2 m) (mcol 3 m)

mtranslate :: (Double, Double) -> Mat3
mtranslate (x', y') = mat3 (1, 0, x') (0, 1, y') (0, 0, 1)

mrotate :: Double -> Mat3
mrotate deg = mat3inv $ mat3 (c, ms, 0) (s, c, 0) (0, 0, 1)
  where
    rds = deg * pi / 180
    c = cos rds
    s = sin rds
    ms = sin (-rds)

mscale :: (Double, Double) -> Mat3
mscale (sx, sy) = mat3 (sx, 0, 0) (0, sy, 0) (0, 0, 1)

mshear :: (Double, Double) -> Mat3
mshear (sx, sy) = mat3 (1, sx, 0) (sy, 1, 0) (0, 0, 1)

mtransform :: [Mat3] -> Mat3
mtransform [] = mat3id
mtransform (m:ms) = mtransform ms *.* m
