{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MonomorphismRestriction #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
-- # LANGUAGE PatternSynonyms  #
{-

i'm sorry for the extensive use of unsafeperformio but not really because for example GL.stringWidth is really a pure function for my uses
and it would very much suck to obtain its value in io function and pass it all the way down to pure function that uses it
these uses of unsafeperformio are much safer than say reading a file with hGetContents which is a standard function 

-}

module Editing where
import Graphics.UI.GLUT.Fonts

import Data.Functor.Indexed
import Data.Type.Equality
import Control.Monad.Indexed
import Data.Time
import System.IO
import System.IO  
import qualified Numeric (showHex, showIntAtBase)
import qualified Control.Lens as Lens
import SDL.Internal.Numbered
import Foreign
import Graphics.GL
import Control.Concurrent
import Data.Function
import Data.Time
import Control.Exception.Base (assert, bracket)
import System.Exit
-- import Data.Sequence
import Debug.Trace
import Control.Monad hiding (return, (>>=), (>>))
import Data.IORef
import Data.Foldable
import qualified Data.Text as T
import Data.Strict.Tuple hiding (snd, uncurry)
import qualified SDL.Font as F 
import qualified Data.Strict.Tuple as S
import Data.Char
import SDL hiding(Point, Rectangle)
import qualified SDL.Raw as Raw
import Hyperbolic
import Control.Monad.IO.Class
import Data.Proxy
import Data.String 
import Prelude hiding (return, (>>=), (>>))
import qualified Prelude as P (return, (>>=), (>>)) 
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
import Linear as L hiding (trace) 
import System.IO.Unsafe
-- import qualified Linear as L
import  Graphics
import Data.Singletons.TH 
$(singletons [d|
  data State = AddingWall | Ground | Quit deriving (Read, Show, Eq)
  |]) 
-- eventQueue = unsafePerformIO $ newChan
-- data Event = User UserEvent GL.Modifiers | Leave
-- data UserEvent = Mouse GL.MouseButton GL.KeyState GL.Position
-- model :: IORef (Environment)
-- windowPos (V2 x y) = set?windowPos (V2 x (height-y))
ifThenElse True a b = a
ifThenElse False a b = b 
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

view = unsafePerformIO $ newIORef $ (  rotateAroundY 0.1 {- (tau/(4-0.1))-}  !*! moveAlongX (-0.1) {- moveAlongZ (-0.1) !*! ) --} !*! L.identity::L.M44 Double)
-- type Angle = Double

mapVertexPixel :: SDL.Point V2 CInt -> L.V2 GLdouble
mapVertexPixel (SDL.P (V2 a b)) = L.V2 ((fromIntegral a)*2/(fromIntegral width) - 1 :: GLdouble) 
                                        (1 - (fromIntegral b)*2/(fromIntegral height) ) --0 1beingAddedWall :: GL.Position -> Mesh 
beingAddedWall :: Angle -> SDL.Point V2 CInt -> L.M44 Double -> Mesh
beingAddedWall a pos view = ({-traceComm "RES POINT = " res `seq` --(GL.Position xi yi) = -} 
                                      (assert (abs rz < 0.04) $ (moveRightTo (fromV4 d) !$ wall::Mesh)::Mesh))::Mesh
   where
         -- x = fromIntegral xi
         -- y = traceComm "y" $ fromIntegral yi
         L.V2 x y =  mapVertexPixel pos
         (L.V4 i j k l) = traceComm "ijkl" $ tran ^. L._z
         c = -(l+i*x+j*y)/k
         tran = persViewMatrix !*! view
         invpv = traceComm "invpv" $ L.inv44  $ traceComm "persViewMatrix" (tran)
         (d@(L.V4 rx ry rz rt)) = {- traceComm "d" $ -} invpv !* if {- traceComm "COND!!!!" $ -} abs (traceComm "K =" k) > 0.00001 then (L.V4 x y c 1) else (L.V4 0 0 1 0) 
         res = (tran !* d, L.V2 x y)
         wall = Mesh  [((0.0, 0.0, 1.0, 1.0), (P.Polygon ) [Point 0 0.01 0 1, 
                                                            Point 0 (-0.01) 0 1, 
                                                            Point 0 (-0.01) 0.01 1,
                                                            Point 0 0.01 0.01 1
                                                            ])]


-- x, y <- (0, 1)
-- rx ry 0 rt = persViewMatrix !* (tran !* (p ^. _v4))
gui :: [Button] 
gui = [Button "wall" (SDL.P $ V2 300 200) SGround SAddingWall ( (StartAddingWall) )]

editorDisplay :: {- forall a c. (Floating a, Ord a, Real a, Coercible Double c, Coercible Double a, Show a)Matri
                                         => -}
             MonadIO m => SState a -> Bool -> m ()
editorDisplay st selection = liftIO $ do
--  liftIO $ print "editorDisplay"

  
  -- state' <- get state
  tran  <- liftIO $ get view
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
  (Mesh env) <- fmap P._mesh $ GL.get model 
  -- print cons1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  
  -- GL.position (GL.Light 0) $= lpos -- saneVertex4 0.3 0.1 0.15 (1::GLfloat)
  mapM_ ( toRaw) (env)--  <> (coerce @Mesh @[((Double, Double, Double, Double), HyperEntity)] $ (let tt =  thatTransformation $  (P.Devi (Point 0.00001 0.0001 0.00001 1) (Abs 0.01 1 0.1) (0.1)) 
                                                                                               -- in traceComm "iden" (tt !*! transposeMink tt) `seq` tt) !$ deviator ))

{-  GL.renderPrimitive GL.Triangles $ do
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
  --    GL.vertex $ GL.Vertex4 (-0.5) (1) (0.5::GLdouble) 1 -}
  (case st of
    SAddingWall -> do
      (m ::SDL.Point V2 CInt) <- getAbsoluteMouseLocation
      mapM_ @[] ( toRaw) $ coerce $ beingAddedWall 0 m  tran
    _ -> return ())::IO ()
  color $ GL.Color4 1 1 (1::GLdouble) 0.1
  -- when selection 
  (get selectionProgramID >>= glUseProgram)

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
  when (not selection) (SDL.glSwapWindow win)
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
                        (L.V4 m n o p)) = [coerceG a, coerceG b, coerceG c, coerceG d, coerceG e, coerceG f, coerceG g, coerceG h, coerceG i, coerceG j, coerceG k, coerceG l, coerceG m, coerceG n, coerceG o, coerceG p]
          (>>=) = (P.>>=)
          return = P.return
          (>>) = (P.>>)

-- editorMouseCallback ::    
-- editorMouseCallback mdata = do
  -- windowPos $ (traceComm "pos") pos 
  -- color $ GL.Color3 0 1 (0::GLdouble)
  -- GL.renderString GL.TimesRoman24 "Lol"
  -- GL.swapBuffers
   -- print "editorMouseCallback" 
   -- mouse $= SDL.P (V2 0 0)
   -- case find (\guiButt -> 
    -- buttRectangle guiButt `contains` SDL.P (V2 0 0) ) gui :: Maybe Button of
     -- Nothing -> state $= Ground
     -- Just (Button _ _ a) -> a
   -- GL.postRedisplay Nothing
editorSpecialCallback _ _ = return ()
editorKeyboardCallback a = return ()
 
-- displayButton :: Button -> ( -> IO ()
type Rectangle = SDL.Rectangle CInt-- ((GL.GLint :!: GL.GLint) :!: (GL.GLint :!: GL.GLint))
{-# SPECIALIZE contains :: Rectangle -> SDL.Point V2 CInt -> Bool #-}
contains :: (Integral a, Show a) => Rectangle -> SDL.Point V2 a -> Bool
contains (SDL.Rectangle (SDL.P (V2 lx ly)) (V2 sx sy)) (SDL.P (V2 x y)) = 
  traceComm "lx" lx <= fromIntegral x && fromIntegral ly <= traceComm "y" y && traceComm "hx" (lx+sx) >= fromIntegral x && traceComm "hx" (ly+sy) >= fromIntegral y

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
  where 
          (>>) = (P.>>)

pattern Pos a b = SDL.P (V2 a b)
displayButton :: Button -> IO ()
displayButton butt@(Button text (SDL.P (V2 a b)) _ _ _) = do
  -- GL.ortho 
  color $ GL.Color4 0 1 (0.5::GLdouble) 1
  (_, h) <- F.size sans text

  color $ GL.Color3 1 1 (1::GLdouble)
  displayRectangle $ buttRectangle butt
  color $ GL.Color3 0 0 (1::GLdouble)
  h <- fmap round $ fontHeight TimesRoman24
  windowPos $ GL.Vertex2 (fromIntegral $ a + margin :: GLDouble) ((fromIntegral $ height - (b + h + margin))) 
  color $ GL.Color3 0 1 (0::GLdouble)
  renderString TimesRoman24 (T.unpack text)
  -- -- windowPos $ GL.Position (a + margin) (b + h + margin)
  -- sur <- F.solid sans (L.V4 0 0 255 255) text
  -- texture <- malloc
  -- glGenTextures 1 texture
  -- nt <- get texture
  -- Graphics.GL.glBindTexture GL_TEXTURE_2D nt
  -- V2 w h <- surfaceDimensions sur
  -- lockSurface sur 
  -- p <- surfacePixels sur
  -- glTexImage2D GL_TEXTURE_2D 0 4 (fromIntegral w) (fromIntegral h) 0 GL_RGBA GL_UNSIGNED_BYTE p
  -- unlockSurface sur 
  -- win <- get  sdlWindow
  -- rend <- get rendererR
  -- text <- createTextureFromSurface rend sur -- FIXME непонятно надо ли освобождать sur
  -- glBindTexture text 
  -- rasterPos 
  -- winsur <- SDL.getWindowSurface win
  -- void $ SDL.surfaceBlit sur Nothing winsur $ Just $ SDL.P $ V2 0 (height - (fromIntegral h))
  color $ GL.Color3 0 1 (0::GLdouble)
 where  
          (>>=) = (P.>>=)
          return::Monad m => a -> m a
          return = P.return
          (>>) = (P.>>)
buttRectangle :: Button -> Rectangle
buttRectangle (Button text (Pos a b) _ _ _) =
  let i = unsafePerformIO $ stringWidth TimesRoman24 $ T.unpack text 
      h = unsafePerformIO $ fontHeight TimesRoman24
      fi :: (Num b, Integral a) => a -> b
      fi = fromIntegral
   in SDL.Rectangle (Pos (a) (fi b)) $ V2 (fi i+2*margin) ({-traceComm "Height"  h `seq`-} (round h + 2*margin)) -- (((a :!: (b )) :!:  (((a + i + 2*margin) :!: (b + fontHeight + 2*margin)))))
margin :: CInt
margin = 3

-- startAddingWall :: Editor Ground AddingWall ()
-- startAddingWall = fix error
-- stopAddingWall :: Editor AddingWall Ground P.Mesh
-- stopAddingWall = undefined
data Button where
  Button :: { _text:: !T.Text, _pos:: !(SDL.Point V2 CInt), _i :: SState i, _o :: SState o, _action::Action i o () } -> Button
data Action :: State -> State -> * -> * where
  Do :: IO a -> Action i i a
  StartAddingWall :: Action Ground AddingWall ()
  AddWall :: Mesh -> Action AddingWall Ground Mesh
  Exit ::  Action a Quit ()
deriving instance Show (Action a b c)
instance Show (IO a) where
  show a = "<IO action>"
instance (Show f) => Show (CommandIxMonad Action d e f) where
  show (CReturn a) = show a
  show (a :? f) = "(" ++ show a ++ ":? <some function>)"

run :: Show c => Editor a b c -> IO c
-- run = print 
run (CReturn a) = P.return a
run ((Do a) :? j) = a P.>>= (\r -> run (j r))   
run (StartAddingWall :? f) = run (f ())
run ((AddWall m) :? f) = (modifyIORef model (Lens.over P.mesh (<> m))) P.>> run (f m)
run ((Exit) :? f) = run (f ())
inject :: Action a b c -> Editor a b c
inject a = a :? CReturn

data CommandIxMonad :: (state -> state -> * -> *) ->
                       (state -> state -> * -> *) where
  CReturn  :: a -> CommandIxMonad c i i a
  (:?)     :: c i j a -> (a -> CommandIxMonad c j k b) ->
                CommandIxMonad c i k b 
instance IxMonad (CommandIxMonad c) where
  ibind k act =  ( case act of
    (CReturn a) ->  (k a)
    (c :? j)    ->  (c :? \ a -> ibind k (j a)))
-- (c :? j) >>= k = (c :? \ a -> ibind k (j a)))
-- a >> b = ibind b a

itraverse_ :: (Foldable t, IxApplicative f) => (a -> f a a b) -> t a -> f a a ()
itraverse_ f = foldr ((*>>) . f) (ireturn ())
{-# INLINE ifor_ #-}
ifor_ :: (Foldable t, IxApplicative f) => t a -> (a -> f a a b) -> f a a ()
ifor_ = flip itraverse_
(>>) :: IxMonad m => m i j a -> m j l b -> m i l b
a >> b =  ((imap (const id) a) `iap` b) -- ( a >>>= \_ -> b)
(>>=) = (>>>=)
return = ireturn
instance IxApplicative (CommandIxMonad c) where
  iap (CReturn f) (CReturn a) = CReturn (f a) -- f >>>= \nf -> a >>>= \na -> ireturn (nf na)
  iap (CReturn f) (c :? j) = (c :? \a -> imap f (j a))
  iap (act :? jf) q = (act :? \a1 -> jf a1 `iap` q) -- const (fix id) (fmap jf act)  
  iap (f :? jf) q = (f :? \a -> jf a `iap` q)
  -- iap f (c :? j) = (c :? \a -> f `iap` j a)
instance IxPointed (CommandIxMonad c) where
  ireturn = CReturn
instance IxFunctor (CommandIxMonad c) where
  imap f (CReturn a) = CReturn (f a) --  >>>= ireturn . f
  imap (f::a->b) ((c::c i j a1) :? (j::a1 -> CommandIxMonad c j k a)) = (c :? \w -> imap f (j w))

eventLoop :: ( MouseButtonEventData -> Editor a b c) -> Editor a b c
eventLoop = fix id

editorLoop :: forall a. SState a -> Editor a Quit () 
editorLoop s = do
  event <- liftIO $ pollEvent  -- :: Editor a a SDL.Event

  case fmap eventPayload event of
    -- WindowShownEvent a -> editorDisplay s >> editorLoop s
    -- WindowExposedEvent a -> editorDisplay s >> editorLoop s
    -- WindowClosedEvent a -> inject Exit
    Just (KeyboardEvent a) -> (keyboardCase a) >> editorLoop s
    Just (TextEditingEvent a) -> return () >> editorLoop s  
    Just (TextInputEvent a) -> return () >> editorLoop s
    Just (MouseMotionEvent a) -> (mouseMCase a) >> editorLoop  s
    Just (MouseButtonEvent a) -> mouseCCase a s (\st ed -> ed >> editorLoop st)
    Just (MouseWheelEvent a) -> return () >> editorLoop s
    Just (QuitEvent) -> inject Exit   
    Just (DropEvent a) -> editorLoop   s
    Just (ClipboardUpdateEvent) -> editorLoop s
    Just _ -> editorLoop s
    Nothing -> editorDisplay s False >> editorLoop s

data SomeState where 
  SS :: SState s ->SomeState
data SomeEditor a c where 
  SE :: SState b -> Editor a b c -> SomeEditor a c
-- SomeEditor a c = (forall b. SState b -> Editor a b c -> d) -> d 
-- instance SingI a => Monad (SomeEditor a) where
  -- return c = SE sing (return c)
  -- (SE st ed) >>= f = 
-- middleButton :: MonadIO m => m Bool
-- middleButton = Raw.getMouseState nullPtr nullPtr P.>>= \ma -> P.return (testBit ma (1))
-- horizontalCircle :: Point a -> a -> [Point a]
showHex :: Int -> String
showHex a
  | a < 0 = showU (2^32 - fromIntegral a) 8
  | True  = showU (fromIntegral a) 8
showU :: Integer -> Int -> String
showU a 0 = ""
showU a n = showU (a `div` 16) (n-1) ++ (Numeric.showHex (a `mod` 16) "")
mouseMCase :: forall m. MonadIO m => MouseMotionEventData -> m ()
mouseMCase (MouseMotionEventData {..}) =  ( 
  -- mb <- middleButton
  -- print mouseMotionEventState
  if (ButtonMiddle `elem` mouseMotionEventState)
    then liftIO (do
        -- putStrLn "ButtonMiddle"
            let (V2 x y) = mouseMotionEventRelMotion
                fromGradi x = (fromIntegral x / 360*tau*7.0/30.0)
            modifyIORef view (((rotateAroundZ (fromGradi (-x))) !*!) . (rotateAroundY (fromGradi (y)) !*!)))
    else do
      int <- selected 0 0
      liftIO $ putStrLn $ "11111111111111111:   " ++ showHex int)
     where (>>) :: m a -> m b -> m b
           (>>) = (P.>>)
           (>>=) :: m a -> (a -> m b) -> m b
           (>>=) = (P.>>=)
    
mouseCCase :: forall a d . MouseButtonEventData -> SState a -> (forall b c. SState b -> Editor a b c -> Editor a Quit d) -> Editor a Quit d -- Editor a Ground SomeState
-- _action :: (SingI i, SingI o) => Action i o () 
mouseCCase (MouseButtonEventData window motion which buttonc clicks pos) SGround f = go gui -- FIXME заменить go на for, mapM или fold*
  where go [] = f SGround (ireturn ())  
        go (b@(Button _ _ i o _action ):bs) =  if buttRectangle b `contains` pos 
          then case testEquality i SGround of
            Just Refl -> f o (inject _action)
            Nothing -> f SGround (ireturn ())
          else go bs
mouseCCase (MouseButtonEventData window motion which ButtonLeft td pos) SAddingWall f = do
      (m ::SDL.Point V2 CInt) <- iliftIO getAbsoluteMouseLocation
      view_ <- get view
      let (mesh::Mesh) = beingAddedWall 0 m view_
      f SGround (inject (AddWall mesh))
mouseCCase _  SAddingWall f = f SAddingWall (ireturn ())
keyboardCase :: MonadIO m => KeyboardEventData -> m () 
keyboardCase (KeyboardEventData {..}) 
  | keyboardEventKeyMotion == Pressed = let
   c = keysymKeycode keyboardEventKeysym
   in liftIO $ case lookup c matricesMove of
    Just f -> modifyIORef view (f 0.1 !*!)  
    Nothing -> P.return () 
  | otherwise = P.return ()

matricesMove :: Floating a => [(Keycode, a -> M44 a)]
matricesMove = {-fmap (\(a, b) -> (a, b (1/cosh a))) -}[(KeycodeW, moveAlongX . negate), (KeycodeS, moveAlongX ),
                           (KeycodeA, moveAlongY . negate), (KeycodeD, moveAlongY ), (KeycodeZ, moveAlongZ . negate), (KeycodeC, moveAlongZ )]

type Editor = CommandIxMonad Action
instance Functor (Editor a a) where
  fmap = imap
instance Applicative (Editor a a) where
  pure = ireturn
  a <*> b = iap a b
instance Monad (Editor a a) where
 return = ireturn 
 a >>= b = a >>>= b
instance MonadIO (Editor a a) where
  liftIO a = inject $ Do a  
iliftIO a = inject $ Do a 
drawE :: Editor a a ()
drawE = error "draw"

addWall :: Editor AddingWall Ground Mesh
addWall = do
  a <- return ()
  case True of
    True -> inject $ AddWall (Mesh [])

selected :: MonadIO m => Int32 -> Int32 -> m Int 
selected xx yy = liftIO $ do
  res <- mallocBytes (4)-- char res[4];
  viewport <- mallocBytes (4*4)-- GLint viewport[4]; 

  editorDisplay SGround True
  glGetIntegerv GL_VIEWPORT viewport 
  v3 <- peek (viewport `plusPtr` (3*4))
  glReadPixels xx (v3 - yy) 1 1 GL_RGBA GL_BYTE res
  ress <- peek res
  print "her"
  return ress
 where
   (>>=) = (P.>>=)
   (>>) = (P.>>)
   return = P.return

-- renderSelection :: MonadIO m => m () 
-- renderSelection = do
  -- glClearColor 0.0 0.0 0.0 (0.0::GLfloat)
  -- glClear (GL_COLOR_BUFFER_BIT .|.  GL_DEPTH_BUFFER_BIT)

  -- //set matrices to identity
  -- ...
  -- // set camera as in the regular rendering function
  -- ....

  -- // use the selection shader
  -- (get selectionProgramID) >>= glUseProgram

  -- //perform the geometric transformations to place the first pawn
  -- ...
  -- // set the uniform with the appropriate color code
  -- (get selectionProgramID) >>= \z -> glProgramUniform1i z codeVarLocation 1
  -- // draw first pawn
  -- ...
  
  -- // repeat the above steps for the remaining objects, using different codes

  -- //don't swap buffers
  -- //glutSwapBuffers();

  -- // restore clear color if needed
  -- glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
-- }
