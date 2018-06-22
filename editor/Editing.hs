{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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
{-# LANGUAGE MonomorphismRestriction #-} 
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
-- # LANGUAGE PatternSynonyms  #
{-

i'm sorry for the extensive use of unsafeperformio but not really because for example GL.stringWidth is really a pure function for my uses
and it would very much suck to obtain its value in io function and pass it all the way down to pure function that uses it
these uses of unsafeperformio are much safer than say reading file with hGetContents which is a standard function 

-}

module Editing where

import System.IO
import Control.Exception.Base (assert)
import System.Exit
-- import Data.Sequence
import Debug.Trace
import Control.Monad
import Data.IORef
import Data.Foldable
import qualified Data.Text as T
import Data.Strict.Tuple hiding (snd, uncurry)
import qualified SDL.Font as F 
import qualified Data.Strict.Tuple as S
import Data.Char
import Hyperbolic
import Data.Proxy
import GHC.Exts (coerce)
import qualified SDL  
import Data.Monoid
import Control.Concurrent
import Control.Lens ((^.), (%~), (&))
import Control.Concurrent.Chan
import Control.Concurrent.MVar 
import Debug.Trace
import Foreign.C.Types
import Graphics.Rendering.OpenGL as GL hiding (Point)
import Physics  as P (Mesh(..), HyperEntity(..), AvatarPosition(..))
import qualified Physics  as P
import Control.Concurrent.Chan
import Linear as L
import System.IO.Unsafe
-- import qualified Linear as L
import  Graphics
-- eventQueue = unsafePerformIO $ newChan
-- data Event = User UserEvent GL.Modifiers | Leave
-- data UserEvent = Mouse GL.MouseButton GL.KeyState GL.Position
-- model :: IORef (Environment)
data State = AddingWall | Ground deriving (Read, Show, Eq)
-- windowPos (V2 x y) = set?windowPos (V2 x (height-y)) 
model = unsafePerformIO $ newIORef $ P.Env ( Mesh $ [
                                             ((0.9, 0.1, 1, 1.0),  
                                             (P.Polygon ) $
                                             map ((\a -> rotateAroundZ (a-0.1) !$ moveAlongZ (-0.1::Double) !$ (Point 0.9999 0 (0.0) 1)) .
                                                  (/4) . --360) .
                                                  (*(tau::Double)) .
                                                  fromIntegral::Integer->Point Double) $ [0, 1, 2, 3 {-59-}])])

            ([]) 
            [] 
            []

view = unsafePerformIO $ newIORef $ (  rotateAroundY (-0.2 {-tau/(4-0.1)-})  !*! {- moveAlongZ (-0.1) !*! moveAlongX (0.01)) --}  L.identity::L.M44 Double)
mouse = unsafePerformIO $ newIORef $ SDL.P (V2 0 0) 
state = unsafePerformIO $ newIORef Ground 

mapVertexPixel :: SDL.Point V2 CInt -> L.V2 GLdouble
mapVertexPixel (SDL.P (V2 a b)) = L.V2 ((fromIntegral a)*2/(fromIntegral width) - 1 :: GLdouble) 
                                        (1 - (fromIntegral b)*2/(fromIntegral height) ) --0 1beingAddedWall :: GL.Position -> Mesh 
beingAddedWall :: SDL.Point V2 CInt -> L.M44 Double -> Mesh
beingAddedWall pos view = traceComm "RES POINT = " res `seq` --(GL.Position xi yi) =  
                                      assert (abs rz < 0.04) $ Mesh $ 
                                                            [((0.0, 0.0, 1.0, 1.0), 
                                                              (P.Polygon ) [Point (rx) ry rz rt, Point rx ry (rz+0.5) rt,  Point rx (ry+0.5) rz rt]
                                                             )
                                                            ]
   where
         -- x = fromIntegral xi
         -- y = traceComm "y" $ fromIntegral yi
         L.V2 x y =  mapVertexPixel pos
         (L.V4 i j k l) = traceComm "ijkl" $ tran ^. L._z
         c = -(l+i*x+j*y)/k
         tran = persViewMatrix !*! view
         invpv = traceComm "invpv" $ L.inv44  $ traceComm "persViewMatrix" (tran)
         (d@(L.V4 rx ry rz rt)) = traceComm "d" $ invpv !* if  traceComm "COND!!!!" $ abs (traceComm "K =" k) > 0.00001 then (L.V4 x y c 1) else (L.V4 0 0 1 0) 
         res  = (tran !* d, L.V2 x y)  
-- x, y <- (0, 1)
-- rx ry 0 rt = persViewMatrix !* (tran !* (p ^. _v4))
gui :: [Button] 
gui = [Button "wall" (SDL.P $ V2 300 200) (print "action" >> state $= AddingWall)]

editorDisplay :: {- forall a c. (Floating a, Ord a, Real a, Coercible Double c, Coercible Double a, Show a)Matri
                                         => -}
                IO ()
editorDisplay  = do
  print "editorDisplay"

  
  state' <- get state
  tran  <- get view
  let     toRaw :: ((Double, Double, Double, Double), HyperEntity ) -> IO ()
          toRaw (col, (P.Polygon list)) = do
                              GL.renderPrimitive GL.Polygon $ do
                                color $ curry4 GL.Color4 $  mapTuple coerceG col  
                                applyNormal list
                                mapM_ transform list
          toRaw (col, (Segment a b)) = do
                              GL.renderPrimitive GL.Lines $ do
                                color $ curry4 (GL.Color4) $ mapTuple coerceG col
                                transform a
                                transform b
          toRaw (col, (HPoint a )) = do
                              GL.renderPrimitive GL.Points $ do
                                color $ curry4 (GL.Color4) $ mapTuple coerceG col
                                transform a
          transform :: Point Double -> IO ()--saneVertex4 Double

          transform p {- (H.Point x y z t) -} =  let (L.V4 x y z t) =  persViewMatrix  !* ( {-traceComm "VVV" $-} tran !* (p ^. _v4)) in --transform p = let (L.V4 x y z t) = transposeMink tran !* toL.V4 p  in 
                    {- when ((x/t)>0) -}
                     do
                     (GL.vertex  $  saneVertex4 (coerce $ x) (coerceG $ y) (coerceG $ z) (coerceG t))
          frame (P.Polygon list) = 
              GL.renderPrimitive GL.LineLoop $ mapM_ transform list
          frame (Segment a b) = return ()
  (Mesh env) <- fmap P.mesh $ GL.get model 
  -- print cons1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  
  GL.position (GL.Light 0) $= lpos -- saneVertex4 0.3 0.1 0.15 (1::GLfloat)
  mapM_ ( toRaw) (env)--  <> (coerce @Mesh @[((Double, Double, Double, Double), HyperEntity)] $ (let tt =  thatTransformation $  (P.Devi (Point 0.00001 0.0001 0.00001 1) (Abs 0.01 1 0.1) (0.1)) 
                                                                                               -- in traceComm "iden" (tt !*! transposeMink tt) `seq` tt) !$ deviator ))

  GL.renderPrimitive GL.Triangles $ do
    let v a b c d s = let L.V4 x y z t = (L.perspective (tau/4) (1024/600) (0.01) (1)) !* L.V4 a b c d in GL.vertex $ traceComm s $ saneVertex4 x y z t
    color $ GL.Color3 1 1 (0::GLdouble)
    v (0) (-0.4) (-0.5-0) 1 "yellow"
    color $ GL.Color3 0 1 (1::GLdouble)
    v (0.5*sin(tau/3)) (-0.4) (-0.5*cos(tau/3)-0) 1 "blue"
    color $ GL.Color3 1 0 (1::GLdouble) 
    v (-0.5*sin(tau/3)) (-0.4) (-0.5*cos(tau/3)-0) 1 "pink"
  -- color $ GL.Color3 1 0 (1::GLdouble)
  -- GL.renderPrimitive GL.Quads $ do
  --    GL.vertex $ GL.Vertex4 (-1::GLdouble) (-1) (-0.5) 1
  --    GL.vertex $ GL.Vertex4 (0.5) (-1::GLdouble) (-0.5) 1
  --    GL.vertex $ GL.Vertex4 (0.5) (0.5) (-0.5::GLdouble) 1
  --    GL.vertex $ GL.Vertex4 (-1) (0.5) (-0.5) (1::GLdouble)
  -- color $ GL.Color3 0 1 (1::GLdouble)
  -- GL.renderPrimitive GL.Quads $ do
  --    GL.vertex $ GL.Vertex4 (-0.5::GLdouble) (-0.5) (0.5) 1
  --    GL.vertex $ GL.Vertex4 (1) (-0.5) (0.5::GLdouble) 1
  --    GL.vertex $ GL.Vertex4 (1) (1) (0.5::GLdouble) 1
  --    GL.vertex $ GL.Vertex4 (-0.5) (1) (0.5::GLdouble) 1 
  uselessName <- GL.get state
  when (uselessName == AddingWall) $ do
    (m ::SDL.Point V2 CInt) <- get mouse
    mapM_ @[] ( toRaw) $ coerce $ beingAddedWall m  tran
  color $ GL.Color4 1 1 (1::GLdouble) 0.1
  GL.renderPrimitive GL.Triangles $ do
    let [x, y, z, t] = [ 0.5, 0, 0, 1] --lpos
    GL.vertex $ saneVertex4 (x-0.01::GLdouble) y z t

    GL.vertex $ saneVertex4 x (y-0.01::GLdouble) z t
    GL.vertex $ saneVertex4 x y (z-0.01::GLdouble) t
  mapM_ (frame.snd) env  
  -- GL.clear [GL.DepthBuffer]
  GL.depthFunc $= Nothing
  GL.depthMask $= GL.Disabled
  -- GL.preservingMatrix $ do 
    -- GL.matrixMode $= GL.Projection 
    -- GL.loadIdentity 
    -- GL.matrixMode $= GL.Modelview 0
    -- GL.loadIdentity

  for_ gui displayButton 

  color $ GL.Color3 0 0 (0::GLdouble)
  GL.depthFunc $= Just GL.Less
  GL.depthMask $= GL.Enabled
  color $ GL.Color3 0 0 (1::GLdouble)
  win <- get sdlWindow
  -- ren <- SDL.createRenderer  sdlWindow defaultRenderer
  SDL.glSwapWindow win
  -- return ()
    where 
          -- tran = tranw
          applyNormal (a:b:c:_) = GL.normal $ GL.Normal3 (coerce x :: GLdouble) (coerce y) (coerce z)
            where
              L.V3 x1 y1 z1 = signorm $ cross ( klein a-klein b ) (klein a - klein c)    
              L.V3 x y z = if z1 < 0 then V3 x1 y1 z1 else negate (V3 x1 y1 z1)
          coerceG a = (coerce a) :: GLdouble
          curry4 f (a,b,c, d)=f a b c d
          uncurry4  f a b c d=f (a,b,c,d)
          mapTuple f (a, b, c, d) = (f a, f  b, f c, f d)
          m44toList (L.V4 (L.V4 a b c d)
                        (L.V4 e f g h)
                        (L.V4 i j k l)
                        (L.V4 m n o p)) = [coerceG a,coerceG  b,coerceG  c,coerceG  d,coerceG  e,coerceG  f,coerceG  g,coerceG  h,coerceG  i,coerceG  j,coerceG  k ,coerceG  l,coerceG  m,coerceG  n,coerceG  o,coerceG  p]
--editorMouseCallback ::  
editorMouseCallback butt buttState pos =  do
  -- windowPos $ (traceComm "pos") pos 
  -- color $ GL.Color3 0 1 (0::GLdouble)
  -- GL.renderString GL.TimesRoman24 "Lol"
  -- GL.swapBuffers
   print "editorMouseCallback" 
   mouse $= pos
   case find (\guiButt -> 
    buttRectangle guiButt `contains` pos ) gui :: Maybe Button of
     Nothing -> state $= Ground
     Just (Button _ _ a) -> a
   -- GL.postRedisplay Nothing
buttRectangle :: Button -> SDL.Rectangle CInt
buttRectangle = error "buttRectangle is undefined" 
editorSpecialCallback _ _ = return ()
editorKeyboardCallback a = return ()
 
data Button = Button { _text:: !T.Text, _pos:: !(SDL.Point V2 CInt), _action::IO () }
-- displayButton :: Button -> ( -> IO ()
type Rectangle = SDL.Rectangle CInt-- ((GL.GLint :!: GL.GLint) :!: (GL.GLint :!: GL.GLint))
fontHeight = 24
margin = 3
contains :: Rectangle -> SDL.Point V2 CInt -> Bool
contains (SDL.Rectangle (SDL.P (V2 lx ly)) (V2 sx sy)) (SDL.P (V2 x y)) = 
  traceComm "lx" lx <= traceComm "x" x && traceComm "ly" ly <= traceComm "y" y && traceComm "hx" (lx+sx) >= x && traceComm "hx" (ly+sy) >= y

mapPixelVertex a b = GL.vertex $  saneVertex4 ((fromIntegral a)*2/(fromIntegral width) - 1 :: GLdouble) 
                                             (1 - (fromIntegral b)*2/(fromIntegral height) ) 0 1
displayRectangle :: Rectangle -> IO ()
displayRectangle (SDL.Rectangle (SDL.P (V2 lx ly)) (V2 sx sy)) = (GL.renderPrimitive GL.Quads $ do
                                                    -- let re@(saneVertex4 x y z t) = saneVertex4 0.5 (-0.5) 0 1 --lpos
                                                    -- GL.vertex $ saneVertex4 (x+0.01::GLdouble) y z t

                                                    -- GL.vertex $ saneVertex4 x (y+0.01::GLdouble) z t
                                                    -- GL.vertex $ saneVertex4 x y (z+0.01::GLdouble) t
                                                    -- GL.vertex $ re

                                                    mapPixelVertex  lx ly

                                                    -- let saneVertex4 x y z t = saneVertex4 0.55 0 0 1 --lpos
                                                    -- GL.vertex $ saneVertex4 (x+0.01::GLdouble) y z t
-- 
                                                    -- GL.vertex $ saneVertex4 x (y+0.01::GLdouble) z t
                                                    -- GL.vertex $ saneVertex4 x y (z+0.01::GLdouble) t                            
                                                    mapPixelVertex  (lx+sx) ly
                                                    mapPixelVertex  (lx+sx) (ly+sy) 
                                                    mapPixelVertex  lx (ly+sy)
                                                    )



displayButton :: Button -> IO ()
displayButton butt@(Button text (SDL.P (V2 a b)) _) = do
  -- GL.ortho 
  color $ GL.Color4 0 1 (0.5::GLdouble) 1
  (_, h) <- F.size sans text

  color $ GL.Color3 0 0 (1::GLdouble)
  -- windowPos $ GL.Position (a + margin) (b + h + margin)
  sur <- F.solid sans (L.V4 0 0 255 255) text
  win <- get  sdlWindow
  winsur <- SDL.getWindowSurface win
  void $ SDL.surfaceBlit sur Nothing winsur $ Just $ SDL.P $ V2 0 (height - (fromIntegral h))
  color $ GL.Color3 0 1 (0::GLdouble)
