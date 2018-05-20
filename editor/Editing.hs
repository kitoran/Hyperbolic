{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-} 
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
-- # LANGUAGE PatternSynonyms  #
module Editing where

import System.IO
import System.Exit
-- import Data.Sequence
import Debug.Trace
import Data.IORef
import Data.Foldable
import Data.Strict.Tuple hiding (snd, uncurry)
import qualified Data.Strict.Tuple as S
import Data.Char
import Hyperbolic
import Data.Proxy
import GHC.Exts
import Control.Concurrent
import Control.Lens ((^.), (%~), (&))
import Control.Concurrent.Chan
import Control.Concurrent.MVar 
import Debug.Trace
import Physics  as P (Mesh(..), HyperEntity(..), AvatarPosition(..))
import qualified Physics  as P

import Linear as L
import System.IO.Unsafe
import qualified Graphics.UI.GLUT as GL
-- import qualified Linear as L
import  Graphics.UI.GLUT (GLdouble, color, ($=), GLfloat, GLint)
ziipWith :: (a -> a -> b) -> [a] -> [b]
ziipWith f list = go list
  where
    -- go :: [a] -> [(a, a)]
    go (a:b:as) = f a b : go (b:as)
    go (b:[]) = [f b $ head list]

-- model :: IORef (Environment)
model = unsafePerformIO $ newIORef $ P.Env ( Mesh $ 
                                             [((1.0, 0.0, 0.0, 1.0),  
                                              (P.Polygon ) $
                                              map ((\a -> rotateAroundZ a !$ (Point 1 0 (-0.1) 1)) .
                                                   (/360.0) .
                                                   (*(tau::Double)) .
                                                   fromIntegral::Integer->Point Double) $ [0..35{-9-}])])

            ([]) 
            [] 
            []

view = unsafePerformIO $ newIORef $ (L.identity::L.M44 Double)

lpos = GL.Vertex4 (-1.4313725157195931) 2.663858068380254e-6 (0.3::GLfloat) 1.8460891382643082 

gui :: [((GL.GLint :!: GL.GLint), Button)] 
gui = [((100 :!: 100), Button "wall")] 
(GL.Size width height) = unsafePerformIO $ GL.get GL.screenSize
editorDisplay :: {- forall a c. (Floating a, Ord a, Real a, Coercible Double c, Coercible Double a, Show a)Matri
                                         => -}
                IO ()
editorDisplay  = do
  siii@(GL.Size w h) <- GL.get GL.screenSize

  tran  <- readIORef view
  let     toRaw :: ((Double, Double, Double, Double), HyperEntity ) -> IO ()
          toRaw (col, (P.Polygon list)) = do
                              GL.renderPrimitive GL.Polygon $ do
                                color $ curry4 GL.Color4 $ mapTuple coerceG col
                                applyNormal list
                                mapM_ transform list
                              -- GL.renderPrimitive GL.Polygon $ do
                              --   color $ Color3 0 0 (1::GLdouble) --curry3 Color3 $ mapTuple coerceG col
                              --   applyNormal list
                              --   mapM_ transformnn list
                              -- GL.renderPrimitive GL.Polygon $ do
                              --   color $ Color3 0 1 (0::GLdouble) --curry3 Color3 $ mapTuple coerceG col
                              --   applyNormal list
                              --   mapM_ transformnp list
                              GL.renderPrimitive GL.Polygon $ do
                                color $ GL.Color4 1 1 (0::GLdouble) 1 --curry3 Color3 $ mapTuple coerceG col
                                applyNormal list
                                mapM_ transformpn list
          toRaw (col, (Segment a b)) = do
                              GL.renderPrimitive GL.Lines $ do
                                color $ curry4 (GL.Color4) $ mapTuple coerceG col
                                transform a
                                transform b
          toRaw (col, (HPoint a )) = do
                              GL.renderPrimitive GL.Points $ do
                                color $ curry4 (GL.Color4) $ mapTuple coerceG col
                                transform a
          transform :: Point Double -> IO ()--GL.Vertex4 Double

          transform p {- (H.Point x y z t) -} =  let (L.V4 x y z t) = tran !* (p ^. _v4)  in --transform p = let (L.V4 x y z t) = transposeMink tran !* toL.V4 p  in 
                    {- when ((x/t)>0) -}
                     do
                     (GL.vertex $ traceShowId $  GL.Vertex4 (coerce $ x) (coerceG $ y) (coerceG $ z) (coerceG t))
          transformpn :: Point Double -> IO ()--GL.Vertex4 Double

          transformpn p {- (H.Point x y z t) -} =  let (L.V4 x y z t) = tran !* ((p & _t %~ id) ^. _v4 )  in --transform p = let (L.V4 x y z t) = transposeMink tran !* toL.V4 p  in 
                    {- when ((x/t)>0) -}
                     do
                     (GL.vertex $ GL.Vertex4 (coerce $ x) (coerceG $ y) (coerceG $ z) (coerceG (negate t)))
          transformnn :: Point Double -> IO ()--GL.Vertex4 Double

          transformnn p {- (H.Point x y z t) -} =  let (L.V4 x y z t) = tran !* ((p & _t %~ negate) ^. _v4 )  in --transform p = let (L.V4 x y z t) = transposeMink tran !* toL.V4 p  in 
                    {- when ((x/t)>0) -}
                     do
                     (GL.vertex $ GL.Vertex4 (coerce $ x) (coerceG $ y) (coerceG $ z) (coerceG (negate t)))
          transformnp :: Point Double -> IO ()--GL.Vertex4 Double

          transformnp p {- (H.Point x y z t) -} =  let (L.V4 x y z t) = tran !* ((p & _t %~ negate) ^. _v4 )  in --transform p = let (L.V4 x y z t) = transposeMink tran !* toL.V4 p  in 
                    {- when ((x/t)>0) -}
                     do
                     (GL.vertex $ GL.Vertex4 (coerce $ x) (coerceG $ y) (coerceG $ z) (coerceG ( t)))
          frame (P.Polygon list) = 
              GL.renderPrimitive GL.LineLoop $ mapM_ transform list
          frame (Segment a b) = return ()
  (Mesh env) <- fmap P.mesh $ GL.get model
  -- print cons1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  
  GL.position (GL.Light 0) $= lpos -- GL.Vertex4 0.3 0.1 0.15 (1::GLfloat)
  mapM_ ( toRaw) env

  color $ GL.Color4 1 1 (1::GLdouble) 0.1
  GL.renderPrimitive GL.Triangles $ do
    let GL.Vertex4 x y z t = GL.Vertex4 0.5 0 0 1 --lpos
    GL.vertex $ GL.Vertex4 (x+0.01::GLdouble) y z t

    GL.vertex $ GL.Vertex4 x (y+0.01::GLdouble) z t
    GL.vertex $ GL.Vertex4 x y (z+0.01::GLdouble) t
  mapM_ (frame.snd) env  
  -- GL.clear [GL.DepthBuffer]
  -- GL.depthFunc $= Nothing
  -- GL.depthMask $= GL.Disabled
  for_ gui (uncurry displayButton) 
  color $ GL.Color3 0 0 (0::GLdouble)
  -- GL.depthFunc $= Just GL.Less
  -- GL.depthMask $= GL.Enabled
  color $ GL.Color3 0 0 (1::GLdouble)
  GL.swapBuffers
  -- return ()
    where 
          -- tran = tranw
          applyNormal (a:b:c:_) = GL.normal $ GL.Normal3 (coerce x :: GLdouble) (coerce y) (coerce z)
            where
              V3 x1 y1 z1 = signorm $ cross ( klein a-klein b ) (klein a - klein c)    
              V3 x y z = if z1 < 0 then V3 x1 y1 z1 else negate (V3 x1 y1 z1)
          coerceG a = (coerce a) :: GLdouble
          curry4 f (a,b,c, d)=f a b c d
          uncurry4  f a b c d=f (a,b,c,d)
          mapTuple f (a, b, c, d) = (f a, f  b, f c, f d)
          m44toList (L.V4 (L.V4 a b c d)
                        (L.V4 e f g h)
                        (L.V4 i j k l)
                        (L.V4 m n o p)) = [coerceG a,coerceG  b,coerceG  c,coerceG  d,coerceG  e,coerceG  f,coerceG  g,coerceG  h,coerceG  i,coerceG  j,coerceG  k ,coerceG  l,coerceG  m,coerceG  n,coerceG  o,coerceG  p]
editorMouseCallback _ _ _ = return ()
editorSpecialCallback _ _ = return ()
editorKeyboardCallback a = return ()

newtype Button = Button { _text::String }
mapPixelVertex a b = GL.vertex $ traceShowId $ GL.Vertex2 ((fromIntegral a)*2/(fromIntegral width) - 1 :: GLdouble) ((fromIntegral b)*2/(fromIntegral height) - 1)
-- displayButton :: Button -> ( -> IO ()
buttRectangle :: (GL.GLint :!: GL.GLint) -> Button -> IO ((GL.GLint :!: GL.GLint) :!: (GL.GLint :!: GL.GLint))
buttRectangle (a :!: b) butt = do 
  h <- fmap round $ GL.fontHeight GL.TimesRoman24
  i <- GL.stringWidth GL.TimesRoman24 $ _text butt
  return (((a :!: (b - h - 20)) :!:  (((a + i + 20) :!: b))))
displayRectangle :: ((GL.GLint :!: GL.GLint) :!: (GL.GLint :!: GL.GLint)) -> IO ()
displayRectangle ((lx :!: ly) :!: (hx :!: hy))= (GL.renderPrimitive GL.Quads $ do
                                                    mapPixelVertex  lx ly
                                                    mapPixelVertex  hx ly
                                                    mapPixelVertex  hx hy 
                                                    mapPixelVertex  lx hy)



displayButton :: (GL.GLint :!: GL.GLint) -> Button -> IO ()
displayButton p@(a :!: b) butt = do
  color $ GL.Color4 0.5 0.5 (0.5::GLdouble) 1
  displayRectangle =<< buttRectangle p butt
  color $ GL.Color3 0 0 (1::GLdouble)
  h <- fmap round $ GL.fontHeight GL.TimesRoman24
  GL.windowPos $ GL.Vertex2 (a + 10) (b - h - 10) 
  color $ GL.Color3 0 1 (0::GLdouble)
  GL.renderString GL.TimesRoman24 (_text butt) 
