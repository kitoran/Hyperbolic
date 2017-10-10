{-# Language NoMonomorphismRestriction, OverloadedStrings,
             MultiParamTypeClasses, DeriveFunctor, DeriveGeneric,
             ScopedTypeVariables, OverloadedLists, MonadFailDesugaring, PackageImports #-}
module Universe (module Hyperbolic, points, {-cameraC, module Projection,
                 viewSegment, module Camera,-}environment,
                  absoluteCircle, Environment(..), parse, level, startPosMatrix,
                  Mesh(..), Obstacles, Obstacle(..), dodecahedronEnv, loadObj) where
--import qualified Projection
import Control.Applicative
import Data.List.Split
import Data.Function ((&))
import qualified Data.Vector as V
import Linear hiding (rotate, distance)
import System.IO 
import qualified Prelude as P
import Prelude hiding (zip, take, drop, zipWith)
import Hyperbolic
import qualified Physics as P
import Physics hiding (Segment)
import Codec.Wavefront hiding (Point, Triangle)
import qualified Codec.Wavefront
import Text.Show.Pretty
--import qualified Camera
import qualified Control.Lens as Lens
import qualified Graphics.Rendering.OpenGL.GL.VertexSpec (Color3(..))
import GHC.Float 
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

macosh = acosh

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
tunnel = concatMap huy (P.take 100 $ iterate (!*! (moveAlongX (-0.09))) identity)
    where huy :: M44 Double -> [Point Double]
          huy = (\m -> fmap (_v4 Lens.%~ (*! m)) circle)
          circle = [Point (sinh 1) (sin $ t*tau/4) (cos $ t*tau/4)  (cosh 3) | t <- [0..4]]
spiral :: [Point Double]
spiral = map (\m -> (Point 0 (sinh 1) 0 (cosh 1)) & _v4 Lens.%~ (*! m) ) 
                  $ P.take 300 $ iterate (!*!(moveAlongX (0.1) !*! rotateAroundX (tau/10)) ) identity
environment :: Mesh (Double, Double, Double) Double
environment = Mesh $ map (\c -> case c of
                          [a, b] -> ((0,0,0), P.Segment a b)
                          [a] -> ((0,0,0), P.Segment a a)) (chunksOf 2 tunnel)

colors :: [(Double, Double, Double)]
colors = [(1, 0, 0), (1, 1, 0), (1, 1, 1), (1, 0, 1), (0, 0, 1), (0, 1, 0), (0, 1, 1), (0.5, 0.5, 1)]
reds = P.repeat (1,0,0)
whites = P.repeat (1,1,1)

octahedron :: P.Environment (Double, Double, Double) Double
octahedron = Env (Mesh $ P.zip (P.take 8 reds) [ Polygon [f, l, u], ( Polygon [f, d, l]), Polygon [f, u, r], Polygon [f, r, d],
              Polygon [b, u, l], Polygon [b, l, d], Polygon [b, r, u], Polygon [b, d, r]]) $ [ Sphere (Point (sinh 0) 0 0.00 (cosh 0)) (1/2)]
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

pentacles = P.zipWith (!$) (P.take 5 $ iterate (!*! rotateAroundZ (tau/5)) identity) (repeat $ moveAlongX (cl - 0.0) !$ 
                                                                                            moveAlongZ (-0) !$ origin)
pentagon c = Env (Mesh $ fmap (\p -> (c, Polygon [(rotateAroundZ (tau/5) !$ p), p, ( moveAlongZ (-0) !$ origin)])) pentacles)
                 (fmap (\p ->  Triangle (rotateAroundZ (tau/5) !$ p) p ( moveAlongZ (-0) !$ origin) 0.05) pentacles)

--pentagon
levelFOne =    pentagon (1, 0, 0) `unionEnv` 
               (foldr unionEnv void $ P.zipWith (!$) (P.take 5 $ iterate (!*! rotateAroundZ (tau/5)) identity) (repeat yellowPentagon)) `unionEnv`
               (foldr unionEnv void $ P.zipWith (!$) (P.take 5 $ iterate (!*! rotateAroundZ (tau/5)) identity) (repeat redPentagon)) `unionEnv`
               (foldr unionEnv void $ P.zipWith (!$) (P.take 4 $ P.drop 1 $ iterate (!*! rotateAroundZ (tau/5)) identity) (repeat greenPentagon)) `unionEnv`
               bluePentagon
         --(foldr unionEnv void $ zipWith (!$) (take 5 $ iterate (!*! rotateAroundZ (tau/5)) identity) (repeat bluePentagon)) 
levelF = let dia = (2*cl + 2*bl + 2*al) 
             radi = cl + bl + al
             levelMovedRadi = (moveAlongX dia !$ levelFOne) 
             levelRotatedOnce = (moveAlongX (-radi) !*! rotateAroundZ (tau/5) !*! moveAlongX (radi)) !$ levelFOne
             levelRotatedTwice = (moveAlongX (-radi) !*! rotateAroundZ (tau*2/5) !*! moveAlongX (radi)) !$ levelFOne
         in levelFOne `unionEnv` 
            (foldr unionEnv void $ P.zipWith (!$) (P.take 5 $ iterate (!*! rotateAroundZ (tau/5)) identity) (repeat (levelRotatedOnce `unionEnv` levelRotatedTwice)))

a, b, c, al, bl, cl :: Double
a = tau/10
b = tau/10
c = tau/4
al = macosh ( cos a / sin b )
bl = macosh ( cos b / sin a )
cl = macosh (cosh al * cosh bl)

yellowPentagon = (moveAlongX (cl) !*! rotateAroundZ (tau/5) !*! moveAlongX (-cl)) !$ pentagon (1, 1, 0)
redPentagon = (moveAlongX (cl) !*! rotateAroundZ (tau*2/5) !*! moveAlongX (-cl)) !$ pentagon (1, 0, 0)
greenPentagon = (moveAlongX (cl) !*! rotateAroundZ (tau*3/5) !*! moveAlongX (-cl)) !$ pentagon (0, 1, 0)
bluePentagon = (moveAlongX (cl) !*! rotateAroundZ (tau*3/5) !*! moveAlongX (-cl)) !$ pentagon (0, 0, 1)


-- level :: Environment Double
-- level = Env (Mesh [((0, 0.5, 1), square)]) $ [(\(HE q w e) -> Triangle q w e 1) square]
--   where square = moveAlongX 3 !$ HE (Point 0 (-sinh 3) (-sinh 3) 14.202662994046431) (Point 0 (sinh 3) (-sinh 3) 14.202662994046431) (Point 0 0 (sinh 3) (cosh 3))
        
-- level = foldr unionEnv (Env (Mesh []) []) $ zipWith (!$) (take 5 $ iterate (!*! rotateAroundZ (tau/5)) identity) (repeat $ moveAlongX 1 !$ octahedron)

unionEnv (Env (Mesh l1) l2) (Env (Mesh m1) m2) = Env (Mesh (l1 ++ m1)) (l2 ++ m2) 

startPosMatrix = identity --V4 (V4 0 0 0 1) (V4 0 1 0 0 ) (V4 0 0 1 0) (V4 1 0 0 (0))

parallelAxiom = Env (Mesh $ P.zip colors [Polygon [a, b, c], Polygon [a, b, e]]) ghost
          where a = Point 0 (-1)  0 1
                b = Point  0 1 0 1
                c = Point (sin (tau/12)) (cos (tau/12)) 0 1
                e = Point (sin (tau*5/12)) (cos (tau*5/12)) 0 1

-- data Environment a = Env { mesh :: Mesh a,
--                           obstacles :: Obstacles a } deriving (Eq, Show,Functor)
-- newtype Mesh a = Mesh [((a, a, a), HyperEntity a)] deriving (Eq, Show,Functor)
-- data HyperEntity a = HE (Point a) (Point a) (Point a) deriving (Eq, Show,Functor)
ghost :: Obstacles a
ghost =  []
void :: P.Environment a b
void = Env (Mesh []) ghost

parse::forall a b. (Read a, Num a, Num b) => String -> P.Environment (b, b, b) a
parse = (`Env` ghost) . Mesh . f . (map (read::String -> a)) . words
  where f :: Num a => [a] -> [((b,b,b) , HyperEntity a)]
        f [] = []
        f (a1:a2:a3:a4:b1:b2:b3:b4:c1:c2:c3:c4:xs) 
               = ((0,0,0), Polygon [(Point a1 a2 a3 a4), (Point b1 b2 b3 b4), (Point c1 c2 c3 c4)] ) : f xs
        f _ = error "mesh is ill-formed"
    {-[Point t 0 0 (sqrt (t+1)) | t <- [-10, -9.9..10]] ++
         [Point 0 t 0 (sqrt (t+1)) | t <- [-10, -9.9..10]] ++
         [Point 0 0 t (sqrt (t+1)) | t <- [-1, -0.99..1]] 
-}

{-[Point 0 0 0 1, Point 1 0 0 (sqrt 2), Point 0 1 0 (sqrt 2), 
                                  Point (-1) 0 0 (sqrt 2), Point 0 (-1) 0 (sqrt 2)]-}

save :: (Show a, Show b) => FilePath -> P.Environment a b -> IO ()
save a e = writeFile a $ ppShow e

dodecahedralValue :: Double
dodecahedralValue = acosh $ ctgACos sinDIv * ctgASin ((cos gamma)/(sin (tau/6)))
  where ctgACos a = a / (sqrt (1 - a*a)) 
        ctgASin a = 1 / ctgACos a
        gamma = asin (1/(2*(sin(tau/10))))

sinDIv = sin (3*tau/20) / sin (tau/6)


dodecahedralAngle :: Double
dodecahedralAngle = 2 * acos sinDIv

dodecahedralPointFront x = moveAlongX x !$ origin

dodecahedralPointSecondUp x = rotateAroundY dodecahedralAngle !$ dodecahedralPointFront x

dodecahedralPointsSecond x = [rotateAroundX a !$ dodecahedralPointSecondUp x | a <- [0, tau/3, negate tau/3]]

dodecahedralPointThirdUpLeft x = rotateAroundY dodecahedralAngle !$ (rotateAroundX (tau/6)) !$ dodecahedralPointSecondUp x
dodecahedralPointThirdUpRight x = rotateAroundY dodecahedralAngle !$ (rotateAroundX (negate tau/6)) !$ dodecahedralPointSecondUp x

dodecahedralPointsThirdUp x = [rotateAroundY dodecahedralAngle !$ rotateAroundX a !$ rotateAroundY (-dodecahedralAngle) !$ dodecahedralPointFront x | a <- [tau/3, negate tau/3]]

--dodecahedralPointsThirdUp = [rotateAroundY dodecahedralAngle !$ rotateAroundX a !$ dodecahedralPointSecondUp | a <- [tau/6, negate tau/6]]

dodecahedralPointsThird x = do
  point <- dodecahedralPointsThirdUp x
  a <- [0, tau/3, negate tau/3]
  return $ rotateAroundX a !$ point

dodecahedralEdgeUpFirst x = P.Segment (dodecahedralPointFront x) (dodecahedralPointSecondUp x)

dodecahedralEdgesUpSecond x = [P.Segment (dodecahedralPointSecondUp x) a | a <- (dodecahedralPointsThirdUp x)]
 
dodecahedralEdgeDownThird x = P.Segment (rotateAroundX (tau/3) !$ (dodecahedralPointThirdUpLeft x)) (rotateAroundX (negate tau/3) !$ (dodecahedralPointThirdUpRight x))

dodecahedralEdgeUpLeftFouth x = P.Segment (dodecahedralPointThirdUpLeft x) (reflectAboutOrigin !$ rotateAroundX (negate tau/3) !$ (dodecahedralPointThirdUpRight x))

dodecahedron  :: Double -> [HyperEntity Double]
dodecahedron x = do
  edge <- sixth
  involution <- [id, (reflectAboutOrigin !$)]
  rotation <- [id, (rotateAroundX (tau/3) !$), (rotateAroundX (negate tau/3) !$)]
  return $ involution $ rotation edge
 where sixth = [dodecahedralEdgeUpFirst x] ++ dodecahedralEdgesUpSecond x ++ [dodecahedralEdgeDownThird x, dodecahedralEdgeUpLeftFouth x]




-- dodecahedron = [dodecahedralEdgeUpFirst
--                ,(head dodecahedralEdgesUpSecond)
--                ,(rotateAroundX (tau/3) !$ dodecahedralEdgeDownThird)
--                , rotateAroundX (negate tau/3) !$ (head $ tail dodecahedralEdgesUpSecond)
--                , (rotateAroundX (negate tau/3) !$ dodecahedralEdgeUpFirst)]

-- dodecahedron :: [HyperEntity Double]
-- dodecahedron = [HPoint $ Point a b c 2 | let w = [1, -1] , a <- w, b<-w, c <- w] 

dodecahedronEnv1 :: Double -> (Double, Double, Double) -> Environment (Double, Double, Double) Double
dodecahedronEnv1 x (r, g, b) = Env (Mesh $ fmap (\a -> ((r, g, b), a)) (dodecahedron x)) ghost
-- (255/255, 171/255, 11/255)

dodecahedronEnv2 :: Double -> Environment (Double, Double, Double) Double
dodecahedronEnv2 x = rotate (dodecahedralPointFront x) (dodecahedralPointSecondUp x) (tau/5) !$ Env (Mesh $ fmap (\a -> ((0/255, 171/255, 255/255), a)) (dodecahedron x)) ghost

dodecahedronEnv x = dodecahedronEnv2 x `unionEnv` (dodecahedronEnv1 x (255/255, 171/255, 11/255))

octacles = P.zipWith (!$) (P.take 8 $ iterate (!*! rotateAroundZ (tau/8)) identity) (repeat $ moveAlongX (hypotenuseByAngles (tau/16) (tau/16)) !$ 
                                                                                                  origin)
octagon ::  Environment (Double, Double, Double) Double
octagon = Env (Mesh [((1.0,0.0,0.0), Polygon octacles)])
                 (fmap (\p ->  Triangle (rotateAroundZ (tau/8) !$ p) p ( moveAlongZ (-0) !$ origin) 0.05) octacles)
-- Env (Mesh [(red, Polygon [p0, p1, p2])]) ([Triangle p0 p1 p2 0.01])
--   where p0 = Point 1.0 0.0 0.0 2.0
--         p1 = rotateAroundZ (tau/3) !$ p0
--         p2 = rotateAroundZ (-tau/3) !$ p0
--         red = (1.0, 0.0, 0.0)
dodecahedronSolid :: Double -> [Obstacle Double]
dodecahedronSolid x = do
  triangle <- sixth
  involution <- [id , (reflectAboutOrigin !$)]
  rotation <- [id , (rotateAroundX (tau/3) !$), (rotateAroundX (negate tau/3) !$)]
  return $ involution $ rotation triangle
 where pentagon :: [Obstacle Double]
       pentagon = [
                     Triangle (dodecahedralPointFront x) (dodecahedralPointSecondUp x) (dodecahedralPointThirdUpLeft x) 0.01
                   , Triangle (dodecahedralPointFront x) (dodecahedralPointThirdUpLeft x) point 0.01
                   , Triangle (dodecahedralPointFront x) point (rotateAroundX (tau/3) !$ (dodecahedralPointSecondUp x)) 0.01
                   ]
       sixth = pentagon ++ fmap (rotate origin (dodecahedralPointSecondUp x) (negate tau/3) !$) pentagon
       point = (rotateAroundX (tau/3) !$ (dodecahedralPointThirdUpRight x)) 

dodecahedronSolidEnv x = Env (Mesh $ fmap (\(Triangle q w e _) -> ((205/255, 121/255, 0/255), Polygon [q, w, e])) (dodecahedronSolid x))
                           ( (dodecahedronSolid x))


instance Monoid (Environment a b) where
  mempty = Env (Mesh []) []
  mappend = unionEnv

spiralCase = foldMap (\a -> moveAlongZ (a/160) !$ rotateAroundZ (tau*a/39.7) !$ triangle) ([260..(350)] :: [Double]) 
  where triangle = Env (Mesh [((255/255, 171/255, 11/255), Polygon [p0, p1, p2]), 
                              ((255/255, 171/255, 11/255), Polygon [p3, p1, p2]), 
                              ((0, 0, 0), P.Segment p0 p1),
                              ((0, 0, 0), P.Segment p1 p3),
                              ((0, 0, 0), P.Segment p3 p2),
                              ((0, 0, 0), P.Segment p2 p0)
                             ]) [ Triangle p0 p1 p2 0.01, Triangle p3 p1 p2 0.01] 
        p0, p1, p2, p3 :: Point Double
        p0 = moveAlongX 0.1 !$ origin
        p1 = moveAlongX 0.6 !$ origin
        p2 = rotateAroundZ (tau/39.7) !$ p0
        p3 = rotateAroundZ (tau/39.7) !$ p1
motion = moveAlongX (-value) !*! rotateAroundZ (-angle) !*! toO
  where 
    toO = getTriangleToOxy (dodecahedralPointSecondUp dodecahedralValue) (dodecahedralPointFront dodecahedralValue) (dodecahedralPointThirdUpLeft dodecahedralValue)  
    value = distance (origin) (Point x1 y1 0 t1)
    Point x1 y1 _ t1 = (toO !$ origin)
    angle = (atan2 y x)/2
    Point x y _ _ = toO !$ (dodecahedralPointThirdUpLeft dodecahedralValue)  

-- level :: Environment (Double, Double, Double) Double
-- level = spiralCase `unionEnv` (motion !$ dodecahedronEnv1 (dodecahedralValue - 0.01) (0,0,0)) `unionEnv` (motion !$ dodecahedronEnv1 (dodecahedralValue + 0.01) (0,0,0)) `unionEnv` (motion !$ dodecahedronSolidEnv dodecahedralValue) --(foldr unionEnv void $ zipWith (!$) (take 50 $ iterate (!*! moveAlongX (al + cl)) identity) (repeat (moveAlongZ (-0.3) !$ pentagon (0, 0, 1))))

level  :: Environment (Double, Double, Double) Double
level = Env (Mesh [(red, Polygon [p0, p1, p2])]) ([Triangle p0 p1 p2 0.01])
  where p0 = Point 1.0 0.0 0.0 2.0
        p1 = rotateAroundZ (tau/3) !$ p0
        p2 = rotateAroundZ (-tau/3) !$ p0
        red = (1.0, 0.0, 0.0)

square = Env (Mesh [(red, Polygon [p0, p1, p2]), (red, Polygon [p0, p2, p3])]) ([Triangle p0 p1 p2 0.01, Triangle p0 p2 p3 0.01])
  where p = Point 1.0 0.0 0.0 2.0
        p0 = rotateAroundZ (tau/8) !$ p
        p1 = rotateAroundZ (tau/4) !$ p0
        p2 = rotateAroundZ (tau/2) !$ p0
        p3 = rotateAroundZ (-tau/4) !$ p0
        red = (1.0, 0.0, 0.0)

line ::  Environment (Double, Double, Double) Double
line = foldMap (\a -> moveAlongX (len*a) !$ square ) ([0..200] :: [Double])
    where p0 = Point 1.0 0.0 0.0 2.0
          p1 = rotateAroundZ (tau/4) !$ p0
          len = distance p0 p1
-- не ебу я как с трансформерами работать и вообще пишут в интернете что IO (Either x y) - это зло
loadObj :: FilePath -> FilePath -> IO (Either String (Environment (Double, Double, Double) Double))
loadObj pathMesh pathObs = do
  meshE <- fromFile pathMesh
  obsE <- fromFile pathObs
  return $ do
    meshObj <- meshE
    obsObj <- obsE
    mesh <- (parseMesh meshObj:: Either String (Mesh (Double, Double, Double) Double))
    obs <- parseObstacles obsObj
    return $ Env mesh obs

parseMesh :: WavefrontOBJ -> Either String (Mesh (Double, Double, Double) Double)
parseMesh obj = fmap Mesh $ mapM toPolygon $ map (elValue) $ V.toList $ objFaces obj
  where 
    toPolygon :: Face -> Either String ((Double, Double, Double), HyperEntity Double)
    toPolygon (Face i1 i2 i3 is) = do
      p1 <- toPoint i1
      p2 <- toPoint i2
      p3 <- toPoint i3
      ps <- mapM toPoint is
      Right $ ((0.5, 0.5, 0.5), Polygon ((p1): (p2): (p3):ps))
    toPoint :: FaceIndex -> Either String (Point Double)
    toPoint i = do
      (Location lx ly lz lw) <- maybe (Left $ "index out of range:" ++ ppShow i ++ "\n" ++ ppShow (objLocations obj)) Right (objLocations obj V.!? (faceLocIndex i-1))
      return $ Point (float2Double lx) (float2Double ly) (float2Double lz) (float2Double lw)

parseObstacles :: WavefrontOBJ -> Either String (Obstacles Double)
parseObstacles obj = mapM toTriangle $ map (elValue) $ V.toList $ objFaces obj
  where 
    toTriangle :: Face -> Either String (Obstacle Double)
    toTriangle (Codec.Wavefront.Triangle i1 i2 i3) = do
      p1 <- toPoint i1
      p2 <- toPoint i2
      p3 <- toPoint i3
      Right $ ( Triangle (p1) (p2) (p3) 0.01)
    toTriangle _ = Left "non-triangle face"
    toPoint :: FaceIndex -> Either String (Point Double)
    toPoint i = do
      (Location lx ly lz lw) <- maybe (Left $ "index out of range:" ++ ppShow i ++ "\n" ++ ppShow (objLocations obj)) Right (objLocations obj V.!? (faceLocIndex i-1))
      return $ Point (float2Double lx) (float2Double ly) (float2Double lz) (float2Double lw)

cube = Env (Mesh car) ghost
  where
    delta = legByAdjacentAndOppositeAngles (tau/8) (tau/12)
    gamma = legByAdjacentAndOppositeAngles (tau/12) (tau/8)
    finite = delta-0.1
    l = [1.0::Double, -1]::[Double]

    car1 = [((1.0, 1, 1), P.Segment (Point (-1.0::Double) x y 2) (Point (1) x y 2)) | x <- l, y <- l] ++
           [((1.0, 1, 1), P.Segment (Point x (-1) y 2) (Point x (1) y 2)) | x <- l, y <- l] ++
           [((1.0, 1, 1), P.Segment (Point x y (-1) 2) (Point x y (1) 2)) | x <- l, y <- l]
    car2 = concat [map (\(c, h) -> (c, commute (moveAlongY gamma !*! moveAlongX delta) (rotateAroundZ x) !$ h)) car1 | x <- [0, tau/6..tau]]
    car = car2 ++ map (\(c, h) -> (c, moveAlongZ (delta*2) !$ h)) car2 ++ map (\(c, h) -> (c, moveAlongZ (delta*(-2)) !$ h)) car2
