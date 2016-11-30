{-# Language NoMonomorphismRestriction, OverloadedStrings,
             MultiParamTypeClasses, DeriveFunctor, DeriveGeneric,
             ScopedTypeVariables #-}
module Universe (module Hyperbolic, points, {-cameraC, module Projection,
                 viewSegment, module Camera,-}enviroment, tau, _ends, Segment(..),
                  absoluteCircle, Environment(..), HyperEntity(..), parse, level, startPosMatrix,
                  Mesh(..), Obstacles(..)) where
--import Projection
import Control.Applicative
import Data.List.Split
import Linear
import Hyperbolic
--import Camera
import Control.Lens hiding (view)

tau::Floating a => a
tau = 2*pi 

--camera :: Projector Double
--camera = Projector  { projectorPosition = Point 0 0 (sinh 2) $ cosh 2,
--                      projectorDirection = Point 0 0 0 1,
--                      projectorHorizontal = Point 1 {-(sinh 1)-} 0 0 $ 1 {-cosh 1-},
--                      projectorVertical = Point 0 1{-(sinh 1)-} 0 $ 1{-cosh 1 -}}
--            where sq2 = sqrt 2

--cameraC = Camera {
--                  cameraPosition = Point 0 0 0 $ 1,
--                  cameraDirection = Point (sinh 1) 0 0 $ cosh 1,
--                  cameraVertical = Point 0 0 (sinh 1) $ cosh 1
--         }

--cameraInf =  Projector  { projectorPosition = Point 0 0 (sinh 2) $ cosh 2,
--                          projectorDirection = Point 0 0 0 1,
--                          projectorHorizontal = Point 1  0 (sinh 2) $ cosh 2 {-cosh 1-},
--                          projectorVertical = Point 0 1 (sinh 2) $ cosh 2 }


data Segment a = Segment !(Point a) !(Point a) deriving (Eq, Show,Functor)
_ends :: Traversal' (Segment a) (Point a)
_ends f (Segment x y) = liftA2 Segment (f x) (f y)
--viewSegment :: Floating a => Camera a -> Segment a -> [(a, a)]
--viewSegment c@(Camera p0' d' v') s@(Segment a b) = [view c a, view c b]

points :: [Point Double]
points = -- [Point (sinh t) 0 0 (cosh t) | t <- [-10, -9.5 .. -0.4]] 
           -- ++[Point 0 (sinh t)  0 (cosh t) | t <- [10, 9 .. 0]]
            [Point (sinh 1) (sin t * sinh r/cosh r) (cos t * sinh r/cosh r)  (cosh 1) |
                                             r <- [0, 0.1.. 3],t <- [0, tau/(2**r)/10 .. tau]]

absoluteCircle :: [Point Double]
absoluteCircle = 
            [Point (sinh 1) (sin t ) (cos t)  (cosh 1) | t <- [0, tau/30 .. tau]]

tunnel :: [Point Double]
tunnel = concatMap huy (take 100 $ iterate (!*! (moveAlongX (-0.09))) identity)
    where huy :: M44 Double -> [Point Double]
          huy = (\m -> fmap (_v4 %~ (*! m)) circle)
          circle = [Point (sinh 1) (sin $ t*tau/4) (cos $ t*tau/4)  (cosh 3) | t <- [0..4]]
spiral :: [Point Double]
spiral = map (\m -> (Point 0 (sinh 1) 0 (cosh 1)) & _v4 %~ (*! m) ) 
                  $ take 300 $ iterate (!*!(moveAlongX (0.1) !*! rotateAroundX (tau/10)) ) identity
enviroment :: [Segment Double]
enviroment = map (\c -> case c of
                          [a, b] -> Segment a b
                          [a] -> Segment a a) (chunksOf 2 tunnel)

colors = [(1, 0, 0), (1, 1, 0), (1, 1, 1), (1, 0, 1), (0, 0, 1), (0, 1, 0), (0, 1, 1), (0.5, 0.5, 1)]
level = Env (Mesh $ zip colors [ HE f l u, ( HE f d l), HE f u r, HE f r d,
              HE b u l, HE b l d, HE b r u, HE b d r]) $ Obs [(Point 0 0 0 1, 1)]
  where f = Point (sinh w) 0 0 (cosh w)
        r = Point 0 (sinh w) 0 (cosh w)
        u = Point 0 0 (sinh w) (cosh w)
        b = Point (sinh (-w)) 0 0 (cosh w)
        l = Point 0 (sinh (-w)) 0 (cosh w)
        d = Point 0 0 (sinh (-w)) (cosh w)
        w = 1/3
startPosMatrix = identityIm

parallelAxiom = Env (Mesh $ zip colors [HE a b c, HE a b e]) ghost
          where a = Point 0 (-1)  0 1
                b = Point  0 1 0 1
                c = Point (sin (tau/12)) (cos (tau/12)) 0 1
                e = Point (sin (tau*5/12)) (cos (tau*5/12)) 0 1

data Environment a = Env { mesh :: Mesh a,
                          obstacles :: Obstacles a } deriving (Eq, Show,Functor)
newtype Mesh a = Mesh [((Double, Double, Double), HyperEntity a)] deriving (Eq, Show,Functor)
data HyperEntity a = HE (Point a) (Point a) (Point a) deriving (Eq, Show,Functor)
newtype Obstacles a = Obs [(Point a, a)] deriving (Eq, Show,Functor)
   -- пока все будет твёрдое и со всеми видимыми рёбрами
ghost = Obs []

parse::forall a. Read a => String -> Environment a
parse = (`Env` ghost) . Mesh . f . (map (read::String -> a)) . words
  where f [] = []
        f (a1:a2:a3:a4:b1:b2:b3:b4:c1:c2:c3:c4:xs) 
               = ((0,0,0),HE (Point a1 a2 a3 a4) (Point b1 b2 b3 b4) (Point c1 c2 c3 c4) ) : f xs
        f _ = error "mesh is ill-formed"
    {-[Point t 0 0 (sqrt (t+1)) | t <- [-10, -9.9..10]] ++
         [Point 0 t 0 (sqrt (t+1)) | t <- [-10, -9.9..10]] ++
         [Point 0 0 t (sqrt (t+1)) | t <- [-1, -0.99..1]] 
-}

{-[Point 0 0 0 1, Point 1 0 0 (sqrt 2), Point 0 1 0 (sqrt 2), 
                                  Point (-1) 0 0 (sqrt 2), Point 0 (-1) 0 (sqrt 2)]-}

