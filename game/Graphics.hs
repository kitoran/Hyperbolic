{-# Language NoMonomorphismRestriction, BangPatterns, TupleSections, TemplateHaskell,
    ScopedTypeVariables, FlexibleContexts, MultiParamTypeClasses, OverloadedStrings, MonoLocalBinds #-}
module Graphics where
import Control.Monad(when)
import Control.Arrow
import Graphics.UI.GLUT as GL
{-(($=), 
                         lineSmooth, 
                         initialDisplayMode, 
                         DisplayMode( WithDepthBuffer, DoubleBuffered), 
                         Cursor( None ), 
                         DisplayCallback, 
                         renderPrimitive,
                         getArgsAndInitialize,
                         createWindow,
                         fullScreen, 
                         depthFunc, 
                         ComparisonFunction( Lequal ),
                         depthBounds,
                         Capability( Enabled ),
                         lineWidth,
                         cursor, 
                         clear,
                         ClearBuffer(ColorBuffer, DepthBuffer),
                         Color3(Color3),
                         color,
                         GLdouble,
                         PrimitiveMode(Triangles, Lines),
                         swapBuffers,
                         vertex,
                         Vertex3(Vertex3),
                         Vector3(..) )-}
import qualified Graphics.Rendering.OpenGL.GLU.Matrix (perspective, lookAt)
import Physics  as P (Mesh(..), HyperEntity(..), AvatarPosition(..))
import qualified Physics  as P
import Hyperbolic as H (box, Point(..), transposeMink, normalizeWass, _v4, klein, origin3, _t, distance, (!$)
  , chDistance, rotateAroundX, andThen, andConsideringThat, toNonPhysicalPoint, getPointToOxzAroundOz, getPointToOxyAroundOx, moveTo, moveRightTo)
import qualified Hyperbolic as H
import Linear hiding (perspective, lookAt, trace, distance)
import qualified Linear as L
import Data.Coerce
import Debug.Trace
import Safe
import Data.List
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.Group
import qualified Data.Text as T
import Text.Show.Pretty
import qualified Data.Set as S
import Formatting
import TextShow
import TextShow.TH
import Data.List (intercalate)
import Graphics.UI.GLUT.Objects --fixme
import qualified System.Random
import qualified Control.Lens
import Console
import Control.Lens ((^.), (%~), (&))
import qualified Data.ByteString as BS
-- import qualified Data.Coerce
-- import qualified Graphics.Rendering.FTGL

import qualified Graphics.Rendering.OpenGL.GL.CoordTrans (loadIdentity)
--import qualified DebugTH
--import qualified Control.Lens
-- --import qualified Debug.Trace 
--В этом модуле находится столько всякой нефункциональности, что от двух маленьких unsafeperformio вреда не будет особого

origin :: Point Double
origin = H.origin
$(deriveTextShow ''V3)
$(deriveTextShow ''Point)
$(deriveTextShow ''V4)
initialiseGraphics ::  IO ()
initialiseGraphics = do
    putStrLn "68"
    initialDisplayMode $= [ WithDepthBuffer, DoubleBuffered]
    putStrLn "70"
    _ <- getArgsAndInitialize
    putStrLn "72"
    _window <- createWindow "Hyperbolic"
    putStrLn "74"
    fullScreen
    putStrLn "76"
    depthFunc $= Just Less
    putStrLn "77"
    depthBounds $= Nothing
    lineSmooth $= Enabled
    polygonOffsetFill $= Enabled
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    polygonOffset $= (-0.2, 1) 
    lineWidth $= 2
    cursor $= FullCrosshair
    lighting           $= Enabled 
    light (Light 0)    $= Enabled
    lightModelAmbient  $= Color4 0.5 0.5 0.5 1 
    -- v <- createShader VertexShader
    -- f <- createShader FragmentShader
    -- f2 <- createShader FragmentShader
    -- vst <- BS.readFile "toon.vert"
    -- fst <- BS.readFile "toon.frag"
    -- fst2 <- BS.readFile "toon2.frag"
    -- shaderSourceBS v $= vst
    -- shaderSourceBS f $= fst
    -- shaderSourceBS f2 $= fst2
    -- compileShader v
    -- compileShader f
    -- compileShader f2
    -- p <- createProgram
    -- attachShader p v
    -- attachShader p f
    -- attachShader p f2
    -- linkProgram p
    -- currentProgram $= Just p 
   
    diffuse (Light 0)  $= Color4 1 1 1 1
    blend              $= Enabled
    -- blendFunc          $= (SrcAlpha, OneMinusSrcAlpha) -- ??????????????????????????
    colorMaterial      $= Just (FrontAndBack, AmbientAndDiffuse)
    -- matrixMode $= Projection
    perspective 45 (1024/600) (0.001) (1.1)
    lookAt (Vertex3 (0::GLdouble) 0 0) (Vertex3 1 (0::GLdouble) (0)) (Vector3 (0::GLdouble) 0 1)

lpos = Vertex4 (-1.4313725157195931) 2.663858068380254e-6 (0.3::GLfloat) 1.8460891382643082 
renderConsole :: T.Text -> IO () 
renderConsole s = do
      h <- fontHeight Helvetica10
      let liness = T.lines s
      go liness 0 h
   where
     go []  _  _ = return ()
     go (f:ff) r h = do
       windowPos $ Vertex3 0 (fromIntegral r*h) (0::GLfloat)
       renderString Helvetica10 $ T.unpack f
       go ff (r+1) h

renderText :: T.Text -> IO () 
renderText s = do
      h <- fontHeight Helvetica10
      let liness = T.lines s
          qwe = length liness
      go liness qwe  h
   where
     go [] 0  h = return ()
     go (f:ff) r h = do
       windowPos $ Vertex3 0 (fromIntegral r*h) (0::GLfloat)
       renderString Helvetica10 $ T.unpack f
       go ff (r-1) h
mareix :: Coercible Double a => M44 a -> T.Text
mareix m = T.unlines $ fmap wer [a, s, d, f]
  where q = (fmap.fmap) coerce m
        wer (V4 q w e r) = T.intercalate "\t" (map (sformat (fixed 3)) [q, w, e, (r::Double)])
        (V4 a s d f) = q
--  -- fixme use DisplayLists
displayGame :: {- forall a c. (Floating a, Ord a, Real a, Coercible Double c, Coercible Double a, Show a)
                                         => -}
                 Console -> Bool -> Mesh -> Bool -> M44 Double -> AvatarPosition -> IO ()
displayGame cons whecons (Mesh env) drawFrame tranw state = do
  matrixMode $= Projection
  -- print cons
  clear [ColorBuffer, DepthBuffer]
  -- putStrLn $ "What displayGame sees:" <> show tranw
  lighting           $= Enabled 
  preservingMatrix $ do
    -- matrixx <- (newMatrix RowMajor $ m44toList tran :: IO (GLmatrix GLdouble))
    -- multMatrix matrixx
    position (Light 0) $= lpos -- Vertex4 0.3 0.1 0.15 (1::GLfloat)
    mapM_ ( toRaw) env

    color $ Color4 1 1 (1::GLdouble) 0.1
    renderPrimitive Triangles $ do
      let Vertex4 x y z t = Vertex4 0.5 0 0 1 --lpos
      vertex $ Vertex4 (x+0.01::GLdouble) y z t
      vertex $ Vertex4 x (y+0.01::GLdouble) z t
      vertex $ Vertex4 x y (z+0.01::GLdouble) t
    when drawFrame $ do
      color $ Color3 0 0 (0::GLdouble)
      mapM_ (frame.snd) env

  preservingMatrix $ do
    color $ Color3 0 0 (1::GLdouble)
    loadIdentity
    matrixMode $= Modelview 2

    loadIdentity 
    -- clear [ColorBuffer, DepthBuffer]
    windowPos $ Vertex3 0 0 (0::GLdouble) 
    lighting           $= Disabled 
    -- texture $= Disabled
    -- renderS1tring Helvetica18 $
    -- windowPos $ Vertex3 0 (h) (0::GLfloat)
    let transform2 :: Point Double -> V4 GLdouble
        transform2 p {- (H.Point x y z t)-} =  let (V4 x y z t) = tran !* ((p & _t %~ negate) ^. _v4 )  in --transform p = let (V4 x y z t) = transposeMink tran !* toV4 p  in 
                    {- when ((x/t)>0) -}
                    V4 (coerce $ x/t) (coerceG $ y/t) (coerceG $ z/t) (1)
        P.Polygon tri = snd $ head env
        [aaa, bbb, ccc] = map transform2 $ tri
    (when whecons
          (renderConsole $ T.intercalate "\n" (line cons : history cons ) <> "\n"))
    -- (when  (not whecons)
    --        (renderText $ T.intercalate "\n" [showt (_pos state !* origin3),
    --                                          (showt $ _height state),
    --                                          showt aaa,
    --                                          "dist " <> showt (distance origin ((inv44 tran) !$ origin)),
    --                                          "chDist " <> showt (chDistance origin ((inv44 tran) !$ origin)),
    --                                          "vect " <> showt ((inv44 tran) !$ origin),
    --                                          "insanity " <> showt (H.insanity tran),
    --                                          mareix tran]))

  swapBuffers
  -- return ()
    where 
          tran = tranw
          toRaw :: ((Double, Double, Double, Double), HyperEntity ) -> IO ()
          toRaw (col, (P.Polygon list)) = do
                              renderPrimitive GL.Polygon $ do
                                color $ curry4 Color4 $ mapTuple coerceG col
                                applyNormal list
                                mapM_ transform list
                              -- renderPrimitive GL.Polygon $ do
                              --   color $ Color3 0 0 (1::GLdouble) --curry3 Color3 $ mapTuple coerceG col
                              --   applyNormal list
                              --   mapM_ transformnn list
                              -- renderPrimitive GL.Polygon $ do
                              --   color $ Color3 0 1 (0::GLdouble) --curry3 Color3 $ mapTuple coerceG col
                              --   applyNormal list
                              --   mapM_ transformnp list
                              renderPrimitive GL.Polygon $ do
                                color $ Color4 1 1 (0::GLdouble) 1 --curry3 Color3 $ mapTuple coerceG col
                                applyNormal list
                                mapM_ transformpn list
          toRaw (col, (Segment a b)) = do
                              renderPrimitive Lines $ do
                                color $ curry4 (Color4) $ mapTuple coerceG col
                                transform a
                                transform b
          toRaw (col, (HPoint a )) = do
                              renderPrimitive Points $ do
                                color $ curry4 (Color4) $ mapTuple coerceG col
                                transform a
          transform :: Point Double -> IO ()--Vertex4 Double

          transform p {- (H.Point x y z t) -} =  let (V4 x y z t) = tran !* (p ^. _v4)  in --transform p = let (V4 x y z t) = transposeMink tran !* toV4 p  in 
                    {- when ((x/t)>0) -}
                     do
                     (vertex $ traceShowId $ Vertex4 (coerce $ x) (coerceG $ y) (coerceG $ z) (coerceG t))
          transformpn :: Point Double -> IO ()--Vertex4 Double

          transformpn p {- (H.Point x y z t) -} =  let (V4 x y z t) = tran !* ((p & _t %~ id) ^. _v4 )  in --transform p = let (V4 x y z t) = transposeMink tran !* toV4 p  in 
                    {- when ((x/t)>0) -}
                     do
                     (vertex $ Vertex4 (coerce $ x) (coerceG $ y) (coerceG $ z) (coerceG (negate t)))
          transformnn :: Point Double -> IO ()--Vertex4 Double

          transformnn p {- (H.Point x y z t) -} =  let (V4 x y z t) = tran !* ((p & _t %~ negate) ^. _v4 )  in --transform p = let (V4 x y z t) = transposeMink tran !* toV4 p  in 
                    {- when ((x/t)>0) -}
                     do
                     (vertex $ Vertex4 (coerce $ x) (coerceG $ y) (coerceG $ z) (coerceG (negate t)))
          transformnp :: Point Double -> IO ()--Vertex4 Double

          transformnp p {- (H.Point x y z t) -} =  let (V4 x y z t) = tran !* ((p & _t %~ negate) ^. _v4 )  in --transform p = let (V4 x y z t) = transposeMink tran !* toV4 p  in 
                    {- when ((x/t)>0) -}
                     do
                     (vertex $ Vertex4 (coerce $ x) (coerceG $ y) (coerceG $ z) (coerceG ( t)))
          applyNormal (a:b:c:_) = normal $ Normal3 (coerce x :: GLdouble) (coerce y) (coerce z)
            where
              V3 x1 y1 z1 = signorm $ cross ( klein a-klein b ) (klein a - klein c)    
              V3 x y z = if z1 < 0 then V3 x1 y1 z1 else negate (V3 x1 y1 z1)
          coerceG a = (coerce a) :: GLdouble
          curry4 f (a,b,c, d)=f a b c d
          uncurry4  f a b c d=f (a,b,c,d)
          mapTuple f (a, b, c, d) = (f a, f  b, f c, f d)
          m44toList (V4 (V4 a b c d)
                        (V4 e f g h)
                        (V4 i j k l)
                        (V4 m n o p)) = [coerceG a,coerceG  b,coerceG  c,coerceG  d,coerceG  e,coerceG  f,coerceG  g,coerceG  h,coerceG  i,coerceG  j,coerceG  k ,coerceG  l,coerceG  m,coerceG  n,coerceG  o,coerceG  p]
          frame (P.Polygon list) = 
              renderPrimitive LineLoop $ mapM_ transform list
          frame (Segment a b) = return ()

deviator :: Mesh -- aaa-aba aba-abb abb-aab aab-aaa
deviator = Mesh [((0.0, 0.0, 1.0, 1), (P.Polygon [aaa, aab, abb, aba])),
                 ((1.0, 0.7, 0.0, 1), (P.Polygon [baa, bba, bbb, bab])),
                 ((0.0, 0.0, 1.0, 1), (P.Polygon [aaa, aba, bba, baa])),
                 ((0.0, 0.0, 1.0, 1), (P.Polygon [aab, bab, bbb, abb])),
                 ((0.0, 1.0, 1.0, 1), (P.Polygon [aab, aaa, baa, bab])),
                 ((0.0, 0.0, 1.0, 1), (P.Polygon [aba, abb, bbb, bba]))]
  where
    r = 0.005
    aaa = H.Point (r) (r) (r) 1
    aab = H.Point (r) (r) (-r) 1
    aba = H.Point (r) (-r) (r) 1
    baa = H.Point (-r) (r) (r) 1
    abb = H.Point (r) (-r) (-r) 1
    bab = H.Point (-r) (r) (-r) 1
    bba = H.Point (-r) (-r) (r) 1
    bbb = H.Point (-r) (-r) (-r) 1
-- selectedDeviator :: Mesh
-- selectedDeviator = let (Mesh a) = deviator in Mesh $ (fmap (\((q, w, e), r) -> ((f q, f w, f e), r))) a
  -- where f a = if a >= (1/3) then 1 else a+2/3
divider :: Mesh 
divider = let (Mesh w) = deviator in Mesh (map (\(_, he) -> ((1.0, 0.0, 1.0, 1.0), he )) w)

viewPort :: AvatarPosition -> M44 Double
viewPort (AP pos height nod _) = H.rotateAroundY (-nod) !*! H.moveAlongZ (-height) !*! (H.m33_to_m44M $ H.transposeMink3 pos)

maapLast :: (a -> a -> b) -> (a -> b) -> [a] -> [b]-- я видел эту функцию где-то
maapLast f g (a:b:as) = f a b:maapLast f g (b:as)
maapLast f g (a:[]) = [g a]
maapLast f g [] = []

maap :: (a -> a -> b) -> [a] -> [b]-- я видел эту функцию где-то
maap f (a:b:as) = f a b:maap f (b:as)
maap f _ = []

toMesh :: [P.Source ] -> [P.Receiver] -> P.LevelState -> (Mesh, [Mesh], Mesh) 
toMesh s r (P.LS (P.AP pos height nod _) mi (P.WS de di) sel) = (rays {-<> inv-}, items, recvs)
  where
    rays = Mesh lines
    (lines, indices) = foldMap mapping s
    mapping (P.Source pos dir) = case eit of
                                  Left abs -> (maapLast (\a b -> ((1, 1, 1, 1), P.Segment a b)) 
                                                        (\ a -> ((1, 1, 1, 1), P.Segment a (H.toNonPhysicalPoint abs) ))
                                                        line, []) --(, P.Segment pos ))
                                  Right i -> (maap (\a b -> ((1, 1, 1, 1), P.Segment a b)) line, [i]) 
      where
        (line, eit) = unfoldRay de r pos dir
                                                   
    recvs = Mesh $ Control.Lens.imap (\ind (P.Receiver li) -> ((if elem ind indices then 1 else 0, 1, 0, 1), P.Polygon li)) r 
    -- inv = case mi of
    --     Nothing -> mempty
    --     Just P.De -> transposeMink (viewPort (P.AP pos height nod 0))!*! H.moveAlongX 0.011 !*! H.moveAlongZ (-0.012) !$ deviator
    items = fmap (\dede -> let dm = deviator in thatTransformation dede !$ dm) $  de
thatTransformation :: P.Deviator -> V4 (V4 Double)
thatTransformation (P.Devi pos dir d) = let move = moveRightTo pos -- если сделать, чтобы одна функция возвращала moveRightTo и moveRightFrom, то меньше вычислений
                                            dirFromStart = (toNonPhysicalPoint $ transposeMink move !$ dir)
                                            turn = (H.getPointToOxyAroundOy `andThen`  getPointToOxzAroundOz) dirFromStart
                                         in (move !*!  transposeMink turn !*! (rotateAroundX d))
                                         -- судя по профилированию, все тормоза происходят тупо когда мы умножаем матрицу на вектор. Решение - сделать рисование в openGL (то есть менее функционатьным и типизированным) или использовать какую-нибудь библиотеку для GPU.
                                         -- второе решение лучше тем более что они планируют убрать матрицы из openGL
-- {- myUnfoldr - практически точная копия unfoldr из base -}
-- myUnfoldr :: (b -> Maybe (a, b)) -> b -> ([a], b)
-- {-# INLINE unfoldr #-} 
-- myUnfoldr f b0 = build (\c n ->
--   let go b = case f b of
--                Just (a, new_b) -> a `c` go new_b
--                Nothing         -> (n, b)
--   in go b0)
deleteNth n xs = let (a, b) = splitAt n xs in a ++ drop 1  b
unfoldRay :: [P.Deviator] -> [P.Receiver] -> H.Point Double -> H.Absolute Double -> ([(H.Point Double)], Either (H.Absolute Double)  Int)
unfoldRay listd listr pos dir = first (pos:) $ go pos dir -- (pos:map fst (go pos dir ), last $ dir : map snd (go pos dir ))
  where go :: H.Point Double -> H.Absolute Double ->  ([(H.Point Double)], Either (H.Absolute Double)  Int)
        go pos dir  = case foldMaybes listd listr pos dir of
                           Infinity -> ([], Left dir)
                           Receiver (p, i) -> ([p], Right i)
                           Deviator (p, newd) -> case go p newd of (ps, re) -> (p:ps, re)  -- (deleteNth i list)

data FoldMaybesResult = Infinity | Receiver (H.Point Double, Int) | Deviator (H.Point Double, H.Absolute Double)

maxFst Nothing Nothing = Infinity -- if f a > f b then a else b
maxFst Nothing (Just (a,c)) = Receiver c
maxFst (Just (a, b)) Nothing = Deviator b 
maxFst (Just (a1, b)) (Just (a2, c)) = if a1 < a2 then Deviator b else Receiver c 
foldMaybes :: [P.Deviator ] -> [P.Receiver] -> H.Point Double -> H.Absolute Double -> FoldMaybesResult
foldMaybes listd lists pos dir = maxFst (minimumByMay (compare `on` fst) $  listDe) (minimumByMay (compare `on` fst) $ listRcv) 
  where listDe :: [(Double, (H.Point Double, H.Absolute Double))]
        listDe = do
                 dev <- listd
                 case functionDe pos dir dev of
                   Nothing -> []
                   Just (dis, diir) -> 
                     return (dis, ((P._devPos) dev, diir)) --]filter (map (\e@(P.Devi poos _ _) -> ) list)
        listRcv :: [(Double, (H.Point Double, Int))]
        listRcv = do
                  (src, i) <- zip lists [0..]
                  case functionRcv pos dir src of
                   Nothing -> []
                   Just (int) -> 
                     return (H.distance pos int, (int, i))

functionRcv :: H.Point Double -> H.Absolute Double -> P.Receiver -> Maybe (H.Point Double)
functionRcv pos dir (P.Receiver ss@(p1:p2:p3:ps)) = if (containsZero projs) then Just $ transposeMink trans !$ intersect else Nothing
  where
    trans = H.getTriangleToOxy p1 p2 p3 
    p@(H.Point px py pz pt) = trans !$ pos
    a@(H.Point ax ay az at) = trans !$ (toNonPhysicalPoint dir)
    intersect = (\pc ac -> pc + (-pz/az) * ac) <$> p <*> a
    L.V2 ix iy = case H.normalizeKlein (intersect) of (H.Point a b c d) -> L.V2 a b
    projs = map ((\p -> case H.normalizeKlein (trans !$ p) of (H.Point a b c d) -> L.V2 (a-ix) (b-iy)) :: H.Point Double -> L.V2 Double) ss
-- (px py pz pt)+a*(ax ay az at) = (smt smt 0 smt) -> a  = -pz/az
functionDe :: H.Point Double -> H.Absolute Double -> P.Deviator -> Maybe (Double, H.Absolute Double)
functionDe pos dir (P.Devi dpos ddir d) = if trace ("newdir = "++show newDir ++ show trans ++ show pos ++ (show $ toNonPhysicalPoint dir)) cond then Just (x/t, newDir) else Nothing
  where

    trans = let move =  H.moveRightTol pos
                dirFromStart = (toNonPhysicalPoint $ invert move !$ dir)
                turn = (box . H.getPointToOxyAroundOy `andThen` box . getPointToOxzAroundOz) dirFromStart
             in (turn <> invert move ) {-}
             ((moveRightTo pos `andConsideringThat`  
                                         (getPointToOxyAroundOx `andThen`  getPointToOxzAroundOz))
                                         (toNonPhysicalPoint dir))-}
    res@(H.Point x y z t) = trans !$ dpos
    newDir = {-H.moveAlongY (0.1) !$ H.rotateAroundZ (H.tau/4) !$ ddir -} invert ( trans) <> box (rotateAroundX (-d)) <> box (H.moveAlongX (H.distance H.origin res)) !$ (H.Abs 0 1 0) -- } trans !$ (H.Abs 0 1 0)
    cond = abs y < 0.001 && abs z < 0.001 && x*t > 0.00001 -- и ещё условие что девиатор правильно повёрнут
-- вызывать гиперболический косинус от гиперболического арккосинуса очень весело
ziip :: [a] -> [(a, a)]
ziip list = go list
  where
    -- go :: [a] -> [(a, a)]
    go (a:b:as) = (a, b) : go (b:as)
    go (b:[]) = [(b, head list)]

{-
ax_1 + by_1 + 1 = 0
ax_2 + by_2 + 1 = 0
a(x_1 - x_2) + b(y_1 - y_2) = 0
a = b(y_2 - y_1)/(x_1 - x_2)
bx_1(y_2 - y_1)/(x_1 - x_2) + by_1 + 1 = 0
bx_1(y_2 - y_1)/(x_1 - x_2) + b(x_1-x_2)y_1 = -1
b(x_1(y_2-y_1) + y_1(x_1-x_2))/(x_1-x_2) = -1
b(x_1(y_2-y_1) - y_1(x_2-x_1))/(x_2-x_1) = 1
b = (x_2 - x_1) / (x_1(y_2-y_1) - y_1(x_2-x_1))
ax_0 + 1 = 0  -- 

x = at + x_1
y = bt + y_1


-}
counterClockwise :: L.V2 Double -> L.V2 Double -> Bool
counterClockwise (L.V2 x1 y1) (L.V2 x2 y2) = case L.cross (L.V3 x1 y1 0) (L.V3 x2 y2 0) of 
                                        (L.V3 _ _ z) -> z > 0
containsZero::[L.V2 Double] -> Bool 
containsZero = all (uncurry counterClockwise) . ziip