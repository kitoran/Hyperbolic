{-# Language NoMonomorphismRestriction, BangPatterns, RecursiveDo, TupleSections, TemplateHaskell #-}
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
--import DebugTH
--import Control.Lens
--import Debug.Trace

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

toFrame::Mesh Double -> [Point Double]
toFrame (Mesh []) = []
toFrame (Mesh ((_, HE x y z):xs) ) = x:y:y:z:z:x:(toFrame (Mesh xs ))
 
display :: Mesh Double -> M44 Double -> DisplayCallback
display (Mesh env) tran = do
  clear [ColorBuffer, DepthBuffer]
  color $ Color3 0 0 (0::GLfloat)
  renderPrimitive Triangles $ mapM_ toRaw env
  color $ Color3 1 1 (1::GLfloat)
  renderPrimitive Lines $ do
     mapM_ transform $ toFrame (Mesh env)
  swapBuffers
    where toRaw :: ((Double, Double, Double), HyperEntity Double) -> IO ()
          toRaw (c1, (HE a b c)) = do
                              transform a
                              transform b
                              transform c
          transform :: Point Double -> IO ()--Vertex4 Double
          transform p = let (V4 x y z t) = toV4 p *! tran in 
                     when (x>0) (vertex $ Vertex3 (y/x*600/1024) (z/x) (x/t)) 
          coerce :: Double -> GLfloat
          coerce  = fromRational.toRational
          uncurry3 f (a,b,c)=f a b c
          mapTuple f (a, b, c) = (f a, f  b, f c)