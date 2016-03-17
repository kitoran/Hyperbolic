{-# Language NoMonomorphismRestriction, OverloadedStrings,
             MultiParamTypeClasses, DeriveFunctor, DeriveGeneric #-}
module Universe (module Hyperbolic, points, cameraC, module Projection, 
                 viewSegment, module Camera,enviroment, tau, _ends, Segment(..)) where
import Projection
import Control.Applicative
import Data.List.Split
import Hyperbolic
import Camera
import Control.Lens hiding (view)

tau::Floating a => a
tau = 2*pi 

camera :: Projector Double
camera = Projector  { projectorPosition = Point 0 0 (sinh 2) $ cosh 2,
                      projectorDirection = Point 0 0 0 1,
                      projectorHorizontal = Point 1 {-(sinh 1)-} 0 0 $ 1 {-cosh 1-},
                      projectorVertical = Point 0 1{-(sinh 1)-} 0 $ 1{-cosh 1 -}}
            where sq2 = sqrt 2

cameraC = Camera {
                  cameraPosition = Point 0 0 0 $ 1,
                  cameraDirection = Point (sinh 1) 0 0 $ cosh 1,
                  cameraVertical = Point 0 0 (sinh 1) $ cosh 1
         }

cameraInf =  Projector  { projectorPosition = Point 0 0 (sinh 2) $ cosh 2,
                          projectorDirection = Point 0 0 0 1,
                          projectorHorizontal = Point 1  0 (sinh 2) $ cosh 2 {-cosh 1-},
                          projectorVertical = Point 0 1 (sinh 2) $ cosh 2 }


data Segment a = Segment !(Point a) !(Point a) deriving (Eq, Show,Functor)
_ends :: Traversal' (Segment a) (Point a)
_ends f (Segment x y) = liftA2 Segment (f x) (f y)
viewSegment :: Floating a => Camera a -> Segment a -> [(a, a)]
viewSegment c@(Camera p0' d' v') s@(Segment a b) = [view c a, view c b]

points :: [Point Double]
points = -- [Point (sinh t) 0 0 (cosh t) | t <- [-10, -9.5 .. -0.4]] 
           -- ++[Point 0 (sinh t)  0 (cosh t) | t <- [10, 9 .. 0]]
            [Point (sinh 1) (sin t * sinh r/cosh r) (cos t * sinh r/cosh r)  (cosh 1) |
                                             r <- [0, 0.1.. 3],t <- [0, tau/(2**r)/10 .. tau]]
enviroment :: [Segment Double]
enviroment = map (\c -> case c of
                          [a, b] -> Segment a b
                          [a] -> Segment a a) (chunksOf 2 points)
    {-[Point t 0 0 (sqrt (t+1)) | t <- [-10, -9.9..10]] ++
         [Point 0 t 0 (sqrt (t+1)) | t <- [-10, -9.9..10]] ++
         [Point 0 0 t (sqrt (t+1)) | t <- [-1, -0.99..1]] 
-}

{-[Point 0 0 0 1, Point 1 0 0 (sqrt 2), Point 0 1 0 (sqrt 2), 
                                  Point (-1) 0 0 (sqrt 2), Point 0 (-1) 0 (sqrt 2)]-}

