{-# Language NoMonomorphismRestriction, BangPatterns, RecursiveDo, TupleSections, TemplateHaskell,
    ScopedTypeVariables, FlexibleContexts, MultiParamTypeClasses #-}
module Graphics where

import Control.Monad(when)
import qualified Graphics.Rendering.OpenGL.GLU.Matrix (perspective, lookAt)
import Graphics.Rendering.OpenGL as GL
import Hyperbolic as H (Point(..), transposeMink, normalizeWass, _v4, klein)
import Linear hiding (perspective, lookAt, trace)
import Data.Coerce
import qualified Control.Lens
import Physics as P
--import qualified Data.ByteString as BS
-- import qualified Data.Coerce
-- import qualified Graphics.Rendering.FTGL

import qualified Graphics.Rendering.OpenGL.GL.CoordTrans (loadIdentity)
--import qualified DebugTH
--import qualified Control.Lens
-- --import qualified Debug.Trace 
--В этом модуле находится столько всякой нефункциональности, что от двух маленьких unsafeperformio вреда не будет особого


initialiseGraphics ::  IO ()
initialiseGraphics = do
    depthFunc $= Just Less
    depthBounds $= Nothing
    lineSmooth $= Enabled
    polygonOffsetFill $= Enabled
    polygonOffset $= (-0.2, 1) 
    lineWidth $= 2
    matrixMode $= Projection
    perspective 45 (1) (0.01) 1
    lookAt (Vertex3 (0::GLdouble) 0 0) (Vertex3 1 (0::GLdouble) (0)) (Vector3 (0::GLdouble) 0 1)

resize :: Int -> Int -> IO ()
resize width height = do
  viewport $= ((Position 0 0), (Size (fromIntegral width) (fromIntegral height)))
  matrixMode $= Projection
  loadIdentity
  perspective 45 (1) (0.01) 1
  lookAt (Vertex3 (0::GLdouble) 0 0) (Vertex3 1 (0::GLdouble) (0)) (Vector3 (0::GLdouble) 0 1)


 -- fixme use DisplayLists
displayGame :: forall a c. (Floating a, Ord a, Real a, Coercible Double c, Coercible Double a)
                                         =>  Mesh (c, c, c) a -> M44 a -> IO ()
displayGame (Mesh env) tran = do
  clear [ColorBuffer, DepthBuffer]
  preservingMatrix $ do
    matrixx <- (newMatrix RowMajor $ m44toList tran :: IO (GLmatrix GLdouble))
    multMatrix matrixx
    mapM_ ( toRaw) env

    color $ Color3 0 0 (1::GLdouble)
    
    color $ Color3 0 0 (0::GLdouble)
    mapM_ (frame.snd) env
    color $ Color3 0 1 (1::GLdouble)
    
    renderPrimitive Lines $ do
                                vertex $ Vertex4 (-50) 0 0 (1::GLdouble)
                                vertex $ Vertex4 50 0 0 (1::GLdouble)
    
    color $ Color3 1 0 (1::GLdouble)
    renderPrimitive Lines $ do
                                vertex $ Vertex4 0 (-50) 0 (1::GLdouble)
                                vertex $ Vertex4 0 50 0 (1::GLdouble)
    color $ Color3 1 1 (0::GLdouble)
    renderPrimitive Lines $ do
                                vertex $ Vertex4 0 0 (-50) (1::GLdouble)
                                vertex $ Vertex4 0 0 50 (1::GLdouble)
  -- color $ Color3 1 1 (1::GLdouble)
    -- renderObject Solid $ Teapot 1
  -- color $ Color3 (1 :: GLdouble) 1 1
  -- color $ Color3 0 0 (0::GLdouble)
  -- flush 

  return ()
    where toRaw :: ((c, c, c), HyperEntity a) -> IO ()
          toRaw (col, (P.Polygon list)) = do
                              renderPrimitive GL.Polygon $ do
                                color $ curry3 Color3 $ mapTuple coerceG col
                                applyNormal list
                                mapM_ transform list
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

          transform (H.Point x y z t) = -- let (V4 x y z t) = tran !* (p ^. _v4)  in --transform p = let (V4 x y z t) = transposeMink tran !* toV4 p  in 
                    {- when ((x/t)>0) -}
                    do
                     (vertex $ Vertex4 (coerce $ x) (coerceG $ y) (coerceG $ z) (coerceG t))
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