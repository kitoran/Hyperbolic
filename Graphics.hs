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
                         GLfloat,
                         PrimitiveMode(Triangles, Lines),
                         swapBuffers,
                         vertex,
                         Vertex3(Vertex3) )
import Universe (Mesh(..), HyperEntity(..), toV4)
import Hyperbolic (Point)
import Linear hiding (perspective, lookAt, trace)
import System.Random

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

toFrame::Floating a => Mesh a -> [Point a]
toFrame (Mesh []) = []
toFrame (Mesh ((_, HE x y z):xs) ) = x:y:y:z:z:x:(toFrame (Mesh xs ))
 
display ::forall a. (Floating a, Ord a, Real a) =>  Mesh a -> M44 a -> DisplayCallback
display (Mesh env) tran = do
  clear [ColorBuffer, DepthBuffer]
  color $ Color3 0 0 (0::GLfloat)
  renderPrimitive Triangles $ mapM_ toRaw env
  color $ Color3 1 1 (1::GLfloat)
  renderPrimitive Lines $ do
     r <- randomIO
     g <- randomIO
     b <- randomIO
     color $ Color3 r g (b::GLfloat)
     mapM_ transform $ toFrame (Mesh env)
  swapBuffers
    where toRaw :: ((a, a, a), HyperEntity a) -> IO ()
          toRaw ((r, g, bl), (HE a b c)) = do
                              color $ Color3 (coerce r) (coerce g) (coerce bl::GLfloat)
                              transform a
                              transform b
                              transform c
          transform :: Point a -> IO ()--Vertex4 Double
          transform p = let (V4 x y z t) = toV4 p *! tran in 
                     when (x>0) (vertex $ Vertex3 (coerce $ y/x*600/1024) (coerce $ z/x) (coerce $ x/t)) 
          coerce :: a -> GLfloat
          coerce  = fromRational.toRational
          uncurry3 f (a,b,c)=f a b c
          mapTuple f (a, b, c) = (f a, f  b, f c)