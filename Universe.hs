{-# Language NoMonomorphismRestriction, OverloadedStrings,
             MultiParamTypeClasses, DeriveFunctor, DeriveGeneric,
             ScopedTypeVariables #-}
module Universe (module Hyperbolic, points, {-cameraC, module Projection,
                 viewSegment, module Camera,-}enviroment, tau, _ends, Segment(..),
                  absoluteCircle, Environment(..), HyperEntity(..), parse, level, startPosMatrix,
                  Mesh(..), Obstacles, Obstacle(..)) where
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
reds = repeat (1,0,0)
whites = repeat (1,1,1)

octahedron :: Environment Double
octahedron = Env (Mesh $ zip (take 8 reds) [ HE f l u, ( HE f d l), HE f u r, HE f r d,
              HE b u l, HE b l d, HE b r u, HE b d r]) $ [ Sphere (Point (sinh 0) 0 0.00 (cosh 0)) (1/2)]
  where f, r, u, b, l, d :: Point Double
        f = (moveAlongX 0 !$) $ Point (sinh w) 0 0 (cosh w)
        r = (moveAlongX 0 !$) $ Point 0 (sinh w) 0 (cosh w)
        u = (moveAlongX 0 !$) $ Point 0 0 (sinh w) (cosh w)
        b = (moveAlongX 0 !$) $ Point (sinh (-w)) 0 0 (cosh w)
        l = (moveAlongX 0 !$) $ Point 0 (sinh (-w)) 0 (cosh w)
        d = (moveAlongX 0 !$) $ Point 0 0 (sinh (-w)) (cosh w)
        w = 1/3
--         f1 = Point (sinh w) 0 0 (cosh w)
--         r1 = Point 0 (sinh w) 0 (cosh w)
--         u1 = Point 0 0 (sinh w) (cosh w)
--         b1 = Point (sinh (-w)) 0 0 (cosh w)
--         l1 = Point 0 (sinh (-w)) 0 (cosh w)
--         d1 = Point 0 0 (sinh (-w)) (cosh w)

pentacles = zipWith (!$) (take 5 $ iterate (!*! rotateAroundZ (tau/5)) identity) (repeat $ moveAlongX (cl - 0.05) !$ origin)
pentatriangles c = Mesh $ fmap (\p -> (c, HE (rotateAroundZ (tau/5) !$ p) p origin)) pentacles
pentagon c = Env (pentatriangles c) ghost

--pentagon
levelF = pentagon (1, 0, 0) `unionEnv` 
         (foldr unionEnv void $ zipWith (!$) (take 5 $ iterate (!*! rotateAroundZ (tau/5)) identity) (repeat yellowPentagon)) `unionEnv`
         (foldr unionEnv void $ zipWith (!$) (take 5 $ drop 1 $ iterate (!*! rotateAroundZ (tau/5)) identity) (repeat redPentagon)) `unionEnv`
         greenPentagon

level = moveAlongZ (-0.3) !$ levelF
a, b, c, al, bl, cl :: Double
a = tau/10
b = tau/8
c = tau/4
al = acosh ( cos a / sin b )
bl = acosh ( cos b / sin a )
cl = acosh (cosh al * cosh bl)

yellowPentagon = (moveAlongX (cl) !*! rotateAroundZ (tau/4) !*! moveAlongX (-cl)) !$ pentagon (1, 1, 0)
redPentagon = (moveAlongX (cl) !*! rotateAroundZ (tau/2) !*! moveAlongX (-cl)) !$ pentagon (1, 0, 0)
greenPentagon = (moveAlongX (cl) !*! rotateAroundZ (tau/2) !*! moveAlongX (-cl)) !$ pentagon (0, 1, 0)

instance Movable Environment where
  tr !$ Env (Mesh m) ob = Env (Mesh $ fmap (\(c, he) -> (c, tr !$ he)) m) (fmap (tr !$) ob)

-- level :: Environment Double
-- level = Env (Mesh [((0, 0.5, 1), square)]) $ [(\(HE q w e) -> Triangle q w e 1) square]
--   where square = moveAlongX 3 !$ HE (Point 0 (-sinh 3) (-sinh 3) 14.202662994046431) (Point 0 (sinh 3) (-sinh 3) 14.202662994046431) (Point 0 0 (sinh 3) (cosh 3))
        
-- level = foldr unionEnv (Env (Mesh []) []) $ zipWith (!$) (take 5 $ iterate (!*! rotateAroundZ (tau/5)) identity) (repeat $ moveAlongX 1 !$ octahedron)

unionEnv (Env (Mesh l1) l2) (Env (Mesh m1) m2) = Env (Mesh (l1 ++ m1)) (l2 ++ m2) 

startPosMatrix = identity --V4 (V4 0 0 0 1) (V4 0 1 0 0 ) (V4 0 0 1 0) (V4 1 0 0 (0))

parallelAxiom = Env (Mesh $ zip colors [HE a b c, HE a b e]) ghost
          where a = Point 0 (-1)  0 1
                b = Point  0 1 0 1
                c = Point (sin (tau/12)) (cos (tau/12)) 0 1
                e = Point (sin (tau*5/12)) (cos (tau*5/12)) 0 1

data Environment a = Env { mesh :: Mesh a,
                          obstacles :: Obstacles a } deriving (Eq, Show,Functor)
newtype Mesh a = Mesh [((a, a, a), HyperEntity a)] deriving (Eq, Show,Functor)
data HyperEntity a = HE (Point a) (Point a) (Point a) deriving (Eq, Show,Functor)
type Obstacles a = [Obstacle a] 
data Obstacle a = Sphere (Point a) a | Triangle (Point a) (Point a) (Point a) a deriving (Eq, Show,Functor)
instance Movable Obstacle where
  tr !$ (Sphere p r) = Sphere (tr !$ p) r
  tr !$ (Triangle q w e r) = Triangle (tr !$ q) (tr !$ w) (tr !$ e) r
   -- пока все будет твёрдое и со всеми видимыми рёбрами
ghost :: Obstacles a
ghost =  []
void :: Environment a
void = Env (Mesh []) ghost
instance Movable HyperEntity where
  a !$ (HE q w e) = HE (a !$ q) (a !$ w) (a !$ e)


parse::forall a. (Read a, Num a) => String -> Environment a
parse = (`Env` ghost) . Mesh . f . (map (read::String -> a)) . words
  where f :: Num a => [a] -> [((a,a,a) , HyperEntity a)]
        f [] = []
        f (a1:a2:a3:a4:b1:b2:b3:b4:c1:c2:c3:c4:xs) 
               = ((0,0,0),HE (Point a1 a2 a3 a4) (Point b1 b2 b3 b4) (Point c1 c2 c3 c4) ) : f xs
        f _ = error "mesh is ill-formed"
    {-[Point t 0 0 (sqrt (t+1)) | t <- [-10, -9.9..10]] ++
         [Point 0 t 0 (sqrt (t+1)) | t <- [-10, -9.9..10]] ++
         [Point 0 0 t (sqrt (t+1)) | t <- [-1, -0.99..1]] 
-}

{-[Point 0 0 0 1, Point 1 0 0 (sqrt 2), Point 0 1 0 (sqrt 2), 
                                  Point (-1) 0 0 (sqrt 2), Point 0 (-1) 0 (sqrt 2)]-}

