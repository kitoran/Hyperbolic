{-# Language NoMonomorphismRestriction, BangPatterns, RecursiveDo, TupleSections, TemplateHaskell,
    ScopedTypeVariables, FlexibleContexts, MultiParamTypeClasses #-}
module Graphics where

import Control.Monad(when)
import Graphics.UI.GLUT 
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
import Graphics.Rendering.OpenGL.GLU.Matrix (perspective, lookAt)
import Physics (Mesh(..), HyperEntity(..))
import Hyperbolic (Point, transposeMink, normalizeWass, _v4)
import Linear hiding (perspective, lookAt, trace)
import Data.Coerce
import System.Random
import Control.Lens
-- import Data.Coerce
-- import Graphics.Rendering.FTGL

import Graphics.Rendering.OpenGL.GL.CoordTrans (loadIdentity)
import Unsafe.Coerce
--import DebugTH
--import Control.Lens
-- --import Debug.Trace 
--В этом модуле находится столько всякой нефункциональности, что от двух маленьких unsafeperformio вреда не будет особого


initialiseGraphics ::  IO ()
initialiseGraphics = do

    initialDisplayMode $= [ WithDepthBuffer, DoubleBuffered]
    _ <- getArgsAndInitialize
    _window <- createWindow "Hyperbolic"
    -- light (Light 0)    $= Enabled
    -- lighting           $= Enabled 
    -- lightModelAmbient  $= Color4 0.5 0.5 0.5 1 
    -- diffuse (Light 0)  $= Color4 1 1 1 1
    -- blend              $= Enabled
    -- blendFunc          $= (SrcAlpha, OneMinusSrcAlpha) 
    -- colorMaterial      $= Just (FrontAndBack, AmbientAndDiffuse)
    fullScreen
    -- mainLoopEvent
  --  (Size x y) <- get screenSize
    --print (Size x y)
    -- consoleSubWindow <- createSubWindow _window (Position 0 (y-300)) (Size x 300)
    depthFunc $= Just Lequal
    depthBounds $= Nothing
    lineSmooth $= Enabled
    lineWidth $= 2
    cursor $= None
    perspective 45 (1024/600) (0.01) 1
    lookAt (Vertex3 (0::GLdouble) 0 0) (Vertex3 1 (0::GLdouble) (0)) (Vector3 (0::GLdouble) 0 1)
 --   return (displayGame _window,


toFrame::Floating a => Mesh c a -> [Point a]
toFrame (Mesh []) = []
toFrame (Mesh ((_, TriangleMesh x y z):xs) ) = x:y:y:z:z:x:(toFrame (Mesh xs ))
toFrame (Mesh ((_, Segment x y):xs) ) = x:y:(toFrame (Mesh xs ))

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

displayGame :: forall a c. (Floating a, Ord a, Real a, Coercible Double c, Coercible Double a)
                                         =>  Mesh (c, c, c) a -> M44 a -> IO ()
displayGame (Mesh env) tran = do
  clear [ColorBuffer, DepthBuffer]
  -- color $ Color3 0 0 (0::GLdouble)
  mapM_ ( toRaw) env
  -- color $ Color3 1 1 (1::GLdouble)
  swapBuffers-- swapBuffers
  return ()
    where toRaw :: ((c, c, c), HyperEntity a) -> IO ()
          toRaw (col, (TriangleMesh a b c)) = do
                              renderPrimitive Triangles $ do
                                color $ curry3 Color3 $ mapTuple coerceG col
                                transform a
                                transform b
                                transform c
          toRaw (col, (Segment a b)) = do
                              renderPrimitive Lines $ do
                                color $ curry3 (Color3) $ mapTuple coerceG col
                                transform a
                                transform b
          toRaw (col, (HPoint a )) = do
                              renderPrimitive Points $ do
                                color $ curry3 (Color3) $ mapTuple coerceG col
                                transform a
          transform :: Point a -> IO ()--Vertex4 Double

          transform p = let (V4 x y z t) = tran !* (p ^. _v4)  in --transform p = let (V4 x y z t) = transposeMink tran !* toV4 p  in 
                    {- when ((x/t)>0) -} (vertex $ Vertex3 (coerceG $ x/t) (coerceG $ (y)/t) (coerceG $ z/t))
          coerceG a = (coerce a) :: GLdouble
          curry3 f (a,b,c)=f a b c
          uncurry3  f a b c=f (a,b,c)
          mapTuple f (a, b, c) = (f a, f  b, f c)

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