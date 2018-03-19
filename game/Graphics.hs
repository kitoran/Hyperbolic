{-# Language NoMonomorphismRestriction, BangPatterns, RecursiveDo, TupleSections, TemplateHaskell,
    ScopedTypeVariables, FlexibleContexts, MultiParamTypeClasses, OverloadedStrings #-}
module Graphics where
import Control.Monad(when)
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
import Graphics.Rendering.OpenGL.GL.Shaders
import Physics  as P (Mesh(..), HyperEntity(..), State(..))
import Riemann as H (Point(..), transposeMink, normalizeWass, _v4, klein, origin3, _t, origin, distance, (!$)
  , cDistance)
import Linear hiding (perspective, lookAt, trace, distance)
import Data.Coerce
import Data.Semigroup
import qualified Data.Text as T
import Text.Show.Pretty
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
    polygonOffset $= (-0.2, 1) 
    lineWidth $= 2
    cursor $= None
    lighting           $= Enabled 
    light (Light 0)    $= Enabled
    lightModelAmbient  $= Color4 0.5 0.5 0.5 1 
    v <- createShader VertexShader
    f <- createShader FragmentShader
    f2 <- createShader FragmentShader
    vst <- BS.readFile "toon.vert"
    fst <- BS.readFile "toon.frag"
    fst2 <- BS.readFile "toon2.frag"
    shaderSourceBS v $= vst
    shaderSourceBS f $= fst
    shaderSourceBS f2 $= fst2
    compileShader v
    compileShader f
    compileShader f2
    p <- createProgram
    attachShader p v
    attachShader p f
    attachShader p f2
    linkProgram p
    -- currentProgram $= Just p 
   
    diffuse (Light 0)  $= Color4 1 1 1 1
    blend              $= Enabled
    -- blendFunc          $= (SrcAlpha, OneMinusSrcAlpha) -- ??????????????????????????
    colorMaterial      $= Just (FrontAndBack, AmbientAndDiffuse)
    matrixMode $= Projection
    perspective 45 (1024/600) (0.01) (100)
    lookAt (Vertex3 (0::GLdouble) 0 0) (Vertex3 1 (0::GLdouble) (0)) (Vector3 (0::GLdouble) 0 1)
--     _ <- getArgsAndInitialize
--     initialDisplayMode $= [ WithDepthBuffer, DoubleBuffered, RGBAMode]
--     initialWindowPosition $= Position 100 100
--     initialWindowSize $= Size 320 320
--     _ <- createWindow "Hyperbolic"
--     displayCallback $= renderScene
--     idleCallback $= Just renderScene
--     reshapeCallback $= Just rc
--     depthFunc $= Just Less
--     light (Light 0)    $= Enabled
--     clearColor $= (Color4 0 0 0 1)
--     lighting           $= Enabled 
--     lightModelAmbient  $= Color4 0.5 0.5 0.5 1 
--     diffuse (Light 0)  $= Color4 1 1 1 1
--     blend              $= Enabled
--     blendFunc          $= (SrcAlpha, OneMinusSrcAlpha) 
--     colorMaterial      $= Just (FrontAndBack, AmbientAndDiffuse)

--     mainLoop
-- rc (Size w h) = do
--     matrixMode $= Projection
--     loadIdentity
--     viewport $= (Position 0 0, Size w h) 
--     perspective 45 ((fromIntegral w) / fromIntegral h) 1 1000
--     matrixMode $= Modelview 1
-- lpos = Vertex4 1 0.5 1 (0::GLfloat)
-- renderScene = do

--   clear [ColorBuffer, DepthBuffer]
--   color $ Color3 (1 :: GLdouble) 1 1
--   position (Light 0) $= Vertex4 0 50 (50) 1  

--   loadIdentity

--   preservingMatrix $ do 
--         translate $ Vector3 (0.5 :: GLfloat) 0.5 (-5)
--         color red
--         renderObject Solid (Sphere' 0.25 20 20)

--   flush
--   -- position (Light 0) $= lpos
--   -- renderObject Solid $ Teapot 1 
--   putStrLn "ww"
--   swapBuffers
--  where green  = Color4 0.8 1.0 0.7 0.9 :: Color4 GLdouble
--        red    = Color4 1.0 0.7 0.8 1.0 :: Color4 GLdouble


 --   return (displayGame _window,



-- displayConsole = do
--   clear [ColorBuffer, DepthBuffer]
--   loadIdentity
--   ortho2D 0 1024 0 600
--   color $ Color3 0 0 (0::GLdouble)
--   renderPrimitive Quads $ do
--     vertex $ Vertex2 (0::GLdouble) 0
--     vertex $ Vertex2 (0::GLdouble) 200
--     vertex $ Vertex2 1024 (200::GLdouble)
--     vertex $ Vertex2 1024 (0::GLdouble)
--   font <- createTextureFont "UbuntuMono-R.ttf"  
--   setFontFaceSize font 24 72
--   color $ Color3 1 1 (1::GLdouble)
--   renderFont font "Hrkko" Graphics.Rendering.FTGL.Front 
--   loadIdentity
--   perspective 45 (1024/600) (0.01) 1
--   lookAt (Vertex3 (0::GLdouble) 0 0) (Vertex3 1 (0::GLdouble) (0)) (Vector3 (0::GLdouble) 0 1)
--   swapBuffers

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
-- displayGame :: forall a c. (Floating a, Ord a, Real a, Coercible Double c, Coercible Double a, Show a)
--                                          =>  Mesh (c, c, c) a -> Bool -> M44 a -> State -> IO ()
-- displayGame (Mesh env) drawFrame tran state = do
--   matrixMode $= Projection
                 
--   clear [ColorBuffer, DepthBuffer]
--   lighting           $= Enabled 
--   preservingMatrix $ do
--     matrixx <- (newMatrix RowMajor $ m44toList tran :: IO (GLmatrix GLdouble))
--     multMatrix matrixx
--     position (Light 0) $= lpos -- Vertex4 0.3 0.1 0.15 (1::GLfloat)
--     mapM_ ( toRaw) env

--     color $ Color3 0 0 (1::GLdouble)
--     renderPrimitive Triangles $ do
--       let Vertex4 x y z t = lpos
--       vertex $ Vertex4 (x+0.01) y z t
--       vertex $ Vertex4 x (y+0.01) z t
--       vertex $ Vertex4 x y (z+0.01) t
--     when drawFrame $ do
--       color $ Color3 0 0 (0::GLdouble)
--       mapM_ (frame.snd) env

--   preservingMatrix $ do
--     color $ Color3 0 0 (1::GLdouble)
--     loadIdentity
--     matrixMode $= Modelview 2

--     loadIdentity 
--     -- clear [ColorBuffer, DepthBuffer]
--     windowPos $ Vertex3 0 0 (0::GLdouble) 
--     lighting           $= Disabled 
--     -- texture $= Disabled
--     -- renderString Helvetica18 $
--     -- windowPos $ Vertex3 0 (h) (0::GLfloat)
--     let transform2 :: Point a -> V4 GLdouble
--         transform2 (H.Point x y z t) = -- let (V4 x y z t) = tran !* (p ^. _v4)  in --transform p = let (V4 x y z t) = transposeMink tran !* toV4 p  in 
--                     {- when ((x/t)>0) -}
--                     V4 (coerce $ x) (coerceG $ y) (coerceG $ z) (coerceG t)
--         P.Polygon tri = snd $ head env
--         [aaa, bbb, ccc] = map transform2 $ tri
--     renderText $  show (_pos state !* origin3) ++ "\n" ++ (show $ _height state) ++ "\n" ++ show aaa ++ "\n" ++ mareix tran

--   swapBuffers
--   -- return ()
--     where toRaw :: ((c, c, c), HyperEntity a) -> IO ()
--           toRaw (col, (P.Polygon list)) = do
--                               renderPrimitive GL.Polygon $ do
--                                 color $ curry3 Color3 $ mapTuple coerceG col
--                                 applyNormal list
--                                 mapM_ transform list
--           toRaw (col, (Segment a b)) = do
--                               renderPrimitive Lines $ do
--                                 color $ curry3 (Color3) $ mapTuple coerceG col
--                                 transform a
--                                 transform b
--           toRaw (col, (HPoint a )) = do
--                               renderPrimitive Points $ do
--                                 color $ curry3 (Color3) $ mapTuple coerceG col
--                                 transform a
--           transform :: Point a -> IO ()--Vertex4 Double

--           transform (H.Point x y z t) = -- let (V4 x y z t) = tran !* (p ^. _v4)  in --transform p = let (V4 x y z t) = transposeMink tran !* toV4 p  in 
--                     {- when ((x/t)>0) -}
--                     do
--                      (vertex $ Vertex4 (coerce $ x) (coerceG $ y) (coerceG $ z) (coerceG t))
--           applyNormal (a:b:c:_) = normal $ Normal3 (coerce x :: GLdouble) (coerce y) (coerce z)
--             where
--               V3 x1 y1 z1 = signorm $ cross ( klein a-klein b ) (klein a - klein c)    
--               V3 x y z = if z1 < 0 then V3 x1 y1 z1 else negate (V3 x1 y1 z1)
--           coerceG a = (coerce a) :: GLdouble
--           curry3 f (a,b,c)=f a b c
--           uncurry3  f a b c=f (a,b,c)
--           mapTuple f (a, b, c) = (f a, f  b, f c)
--           m44toList (V4 (V4 a b c d)
--                         (V4 e f g h)
--                         (V4 i j k l)
--                         (V4 m n o p)) = [coerceG a,coerceG  b,coerceG  c,coerceG  d,coerceG  e,coerceG  f,coerceG  g,coerceG  h,coerceG  i,coerceG  j,coerceG  k ,coerceG  l,coerceG  m,coerceG  n,coerceG  o,coerceG  p]
--           frame (P.Polygon list) = 
--               renderPrimitive LineLoop $ mapM_ transform list
--           frame (Segment a b) = return ()
 -- fixme use DisplayLists
displayGame :: {- forall a c. (Floating a, Ord a, Real a, Coercible Double c, Coercible Double a, Show a)
                                         => -}
                 Console -> Bool -> Mesh (Double, Double, Double) Double -> Bool -> M44 Double -> State -> IO ()
displayGame cons whecons (Mesh env) drawFrame tranw state = do
  matrixMode $= Projection
  -- print cons
  clear [ColorBuffer, DepthBuffer]
  lighting           $= Enabled 
  preservingMatrix $ do
    -- matrixx <- (newMatrix RowMajor $ m44toList tran :: IO (GLmatrix GLdouble))
    -- multMatrix matrixx
    position (Light 0) $= lpos -- Vertex4 0.3 0.1 0.15 (1::GLfloat)
    mapM_ ( toRaw) env

    color $ Color3 0 0 (1::GLdouble)
    renderPrimitive Triangles $ do
      let Vertex4 x y z t = lpos
      vertex $ Vertex4 (x+0.01) y z t
      vertex $ Vertex4 x (y+0.01) z t
      vertex $ Vertex4 x y (z+0.01) t
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
    (when  (not whecons)
           (renderText $ T.intercalate "\n" [showt (_pos state !* origin3),
                                             (showt $ _height state),
                                             showt aaa,
                                             "dist " <> showt (distance origin ((inv44 tran) !$ origin)),
                                             "cDist " <> showt (cDistance origin ((inv44 tran) !$ origin)),
                                             "vect " <> showt ((inv44 tran) !$ origin),
                                             mareix tran]))

  swapBuffers
  -- return ()
    where 
          tran = tranw
          toRaw :: ((Double, Double, Double), HyperEntity Double) -> IO ()
          toRaw (col, (P.Polygon list)) = do
                              renderPrimitive GL.Polygon $ do
                                color $ curry3 Color3 $ mapTuple coerceG col
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
                                color $ Color3 1 1 (0::GLdouble) --curry3 Color3 $ mapTuple coerceG col
                                applyNormal list
                                mapM_ transformpn list
          toRaw (col, (Segment a b)) = do
                              renderPrimitive Lines $ do
                                color $ curry3 (Color3) $ mapTuple coerceG col
                                transform a
                                transform b
          toRaw (col, (HPoint a )) = do
                              renderPrimitive Points $ do
                                color $ curry3 (Color3) $ mapTuple coerceG col
                                transform a
          transform :: Point Double -> IO ()--Vertex4 Double

          transform p {- (H.Point x y z t) -} =  let (V4 x y z t) = tran !* (p ^. _v4)  in --transform p = let (V4 x y z t) = transposeMink tran !* toV4 p  in 
                    {- when ((x/t)>0) -}
                     do
                     (vertex $ Vertex4 (coerce $ x) (coerceG $ y) (coerceG $ z) (coerceG t))
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
          curry3 f (a,b,c)=f a b c
          uncurry3  f a b c=f (a,b,c)
          mapTuple f (a, b, c) = (f a, f  b, f c)
          m44toList (V4 (V4 a b c d)
                        (V4 e f g h)
                        (V4 i j k l)
                        (V4 m n o p)) = [coerceG a,coerceG  b,coerceG  c,coerceG  d,coerceG  e,coerceG  f,coerceG  g,coerceG  h,coerceG  i,coerceG  j,coerceG  k ,coerceG  l,coerceG  m,coerceG  n,coerceG  o,coerceG  p]
          frame (P.Polygon list) = 
              renderPrimitive LineLoop $ mapM_ transform list
          frame (Segment a b) = return ()

-- display :: forall a. (Floating a, Ord a, Real a) =>  Mesh a -> Point a -> a -> a -> DisplayCallback
-- display (Mesh env) tran = do
--   clear [ColorBuffer, DepthBuffer]
--   color $ Color3 0 0 (0::GLdouble)
--   renderPrimitive Triangles $ mapM_ toRaw env
--   color $ Color3 1 1 (1::GLdouble)
--   renderPrimitive Lines $ do
--      r <- randomIO
--      g <- randomIO
--      b <- randomIO
--      color $ Color3 r g (b::GLdouble)
--      mapM_ transform $ toFrame (Mesh env)
--   swapBuffers
--     where toRaw :: ((a, a, a), HyperEntity a) -> IO ()
--           toRaw ((r, g, bl), (HE a b c)) = do
--                               color $ Color3 (coerce r) (coerce g) (coerce bl::GLdouble)
--                               transform a
--                               transform b
--                               transform c
--           transform :: Point a -> IO ()--Vertex4 Double
--           transform p = let (V4 x y z t) = toV4 p *! tran in --transform p = let (V4 x y z t) = transposeMink tran !* toV4 p  in 
--                      when (x>0) (vertex $ Vertex3 (coerce $ y/x*600/1024) (coerce $ z/x) (coerce $ x/t)) 
--           coerce :: a -> GLdouble
--           coerce  = fromRational.toRational
--           uncurry3 f (a,b,c)=f a b c
--           mapTuple f (a, b, c) = (f a, f b, f c)