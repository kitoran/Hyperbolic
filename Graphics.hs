{-# Language NoMonomorphismRestriction, BangPatterns, RecursiveDo, TupleSections, TemplateHaskell,
    ScopedTypeVariables #-}
module Graphics where

import Control.Monad(when)
import Graphics.UI.GLUT (($=), 
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
                         Vector3(..) )
import Graphics.Rendering.OpenGL.GLU.Matrix (perspective, lookAt)
import Universe (Mesh(..), HyperEntity(..), toV4)
import Hyperbolic (Point, transposeMink, normalizeWass)
import Linear hiding (perspective, lookAt, trace)
import System.Random

-- import Data.Coerce


import Unsafe.Coerce
--import DebugTH
--import Control.Lens
-- --import Debug.Trace

initialiseGraphics :: IO ()
initialiseGraphics = do
    initialDisplayMode $= [ WithDepthBuffer, DoubleBuffered]
    _ <- getArgsAndInitialize
    _window <- createWindow "Hyperbolic"
    fullScreen
    depthFunc $= Just Lequal
    depthBounds $= Nothing
    lineSmooth $= Enabled
    lineWidth $= 2
    cursor $= None
    perspective 45 (1024/600) (0.01) 1
    lookAt (Vertex3 (0::GLdouble) 0 0) (Vertex3 1 (0::GLdouble) (0)) (Vector3 (0::GLdouble) 0 1)
    

toFrame::Floating a => Mesh a -> [Point a]
toFrame (Mesh []) = []
toFrame (Mesh ((_, HE x y z):xs) ) = x:y:y:z:z:x:(toFrame (Mesh xs ))
 
display ::forall a. (Floating a, Ord a, Real a) =>  Mesh a -> M44 a -> DisplayCallback
display (Mesh env) tran = do
  clear [ColorBuffer, DepthBuffer]
  color $ Color3 0 0 (0::GLdouble)
  mapM_ (renderPrimitive Triangles .  toRaw) env
  color $ Color3 1 1 (1::GLdouble)
  -- renderPrimitive Lines $ do
  --    let r = 1
  --        g = 1
  --        b = 1
  --    -- g <- 1
  --    -- b <- 1
  --    color $ Color3 r g (b::GLdouble)
  --    -- let randC = do
  --    --             r <- randomIO
  --    --             g <- randomIO
  --    --             b <- randomIO
  --    --             color $ Color3 r g (b::GLdouble)
  --    mapM_ ( transform ) $ toFrame (Mesh env)
  swapBuffers
    where toRaw :: ((a, a, a), HyperEntity a) -> IO ()
          toRaw ((r, g, bl), (HE a b c)) = do
                              color $ Color3 (coerce r) (coerce g) (coerce bl::GLdouble)
                              transform a
                              transform b
                              transform c
          transform :: Point a -> IO ()--Vertex4 Double
          transform p = let (V4 x y z t) = tran !* toV4 p  in --transform p = let (V4 x y z t) = transposeMink tran !* toV4 p  in 
                    {- when ((x/t)>0) -} (vertex $ Vertex3 (coerce $ x/t) (coerce $ (y)/t) (coerce $ z/t))
          coerce :: a -> GLdouble
          coerce  = unsafeCoerce
          uncurry3 f (a,b,c)=f a b c
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