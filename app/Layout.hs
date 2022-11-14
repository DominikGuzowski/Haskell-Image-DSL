module Layout where

import Data.Array

data Point =
  Point
    { x :: Double
    , y :: Double
    }
  deriving (Show, Ord, Eq)

type Points = Array Int Point

data Bounds =
  Bounds
    { minBP :: Point
    , maxBP :: Point
    }

boundsAsPointsList :: Bounds -> Points
boundsAsPointsList (Bounds b1 b2) = toPointArray [lP b1, lP b2]

boundedCorners :: Bounds -> Points
boundedCorners (Bounds (Point x1 y1) (Point x2 y2)) =
  toPointArray [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]

getBoundingCorners :: Bounds -> (Point, Point, Point, Point)
getBoundingCorners (Bounds (Point x1 y1) (Point x2 y2)) =
  (point (x1, y1), point (x2, y1), point (x2, y2), point (x1, y2))

midPoint :: Point -> Point -> Point
midPoint (Point x1 y1) (Point x2 y2) = Point ((x1 + x2) / 2) ((y1 + y2) / 2)

point :: (Double, Double) -> Point
point (x, y) = Point x y

lP :: Point -> (Double, Double)
lP (Point a b) = (a, b)

toPointArray :: [(Double, Double)] -> Points
toPointArray points = listArray (0, length ps - 1) ps
  where
    ps = [(point pair) | pair <- points]

type Vector = Point

vec = Point
