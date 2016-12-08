{-# Language TemplateHaskell, ScopedTypeVariables, NoMonomorphismRestriction, DataKinds #-}
module Main where

import Data.IORef(IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Debug.Trace
import Data.CReal
import System.IO.Unsafe -- это так пока поиграться, потом уберу наверное
import Data.Time.Clock(UTCTime(UTCTime), getCurrentTime, diffUTCTime)
import Control.Monad(when)
import Control.Applicative(liftA2)
import Data.List((++))
import Graphics.UI.GLUT as GL
    (    Size(Size)
    ,    get
    ,    screenSize
    ,    ($=)
    ,    Position(Position)
    ,    keyboardCallback
    ,    displayCallback
    ,    passiveMotionCallback
    ,    pointerPosition
    ,    mainLoop
    )
import Linear ((!*!), M44, inv44)
import Reactive.Banana.Frameworks(newAddHandler,
                                  fromAddHandler, 
                                  AddHandler, 
                                  Handler, 
                                  compile, 
                                  actuate,
                                  MomentIO,
                                  reactimate)
import Reactive.Banana.Combinators -- (accumE, Event, filterJust, unions, filterE, accumB, Behavior, (<@>)) 

import Hyperbolic 
        ( rotateAroundZ
        , rotateAroundY
        , moveAlongZ
        , moveAlongX
        , moveAlongY
        , identityIm
        , insanity
        , pretty
        , invAroundZ
        , (!$)
        , origin
        , distance
        , commute
        )
import Graphics as G (initialiseGraphics, display) 
import DebugTH(prettyR, prettyV)
--import Physics
import Universe (level, startPosMatrix, Environment(..), tau)
import Physics
import GHC.TypeLits


--the unsafePerformIO hack выглядит понятнее гораздо, чем FRP. Можно передавать состояние, конечно, параметрами, но мы этого делать не будем (пока, может потом)
currentMatrix :: IORef (M44 Double)
currentMatrix = unsafePerformIO $ newIORef (identityIm)
currentAngle :: IORef Double
currentAngle = unsafePerformIO $ newIORef (0)

main :: IO ()
main = do
    initialiseGraphics
    (addKeyboard, fireKeyboard) <- (newAddHandler::IO (AddHandler Char, Handler Char))
    (addMouse, fireMouse) <- (newAddHandler::IO (AddHandler (Int, Int), Handler (Int, Int)))
    last <- newIORef $ UTCTime (toEnum 0) 1
    (Size width' height') <- get screenSize 
    let width = fromIntegral width'
    let height = fromIntegral height'

    let network = networkDescription level (width, height) addKeyboard addMouse
    compile network >>= actuate 
    keyboardCallback $= (Just $ \a _ -> fireKeyboard a)
    passiveMotionCallback $= (Just $ (\(Position x y) -> do
        last' <- readIORef last
        now <- getCurrentTime
        when (now `diffUTCTime ` last' > 0.04)
            (do 
             writeIORef last now 
             fireMouse (fromEnum x,fromEnum y)
             pointerPosition $= (Position (width'`div`2) (height'`div`2)))
        ))
    displayCallback $= fireMouse ((fromEnum width'`div`2), (fromEnum height'`div`2))
    redraw (mesh level) 
    fireMouse (width`div`2, height`div`2)
    fireMouse (width`div`2, height`div`2)
    fireMouse (width`div`2, height`div`2)
    fireMouse (width`div`2, height`div`2)
    mainLoop

processKeyboard :: Char -> IO ()
processKeyboard c = case lookup c matrices of
    Nothing -> when (c == 'n') processToOrigin
    Just a -> processMove a

processToOrigin = writeIORef currentMatrix identityIm >> writeIORef currentAngle 0
processMove a = modifyIORef currentMatrix (\m -> m !*! commute m a)
processMouse (x, y) = do
    (Size width' height') <- get screenSize 
    let width = fromIntegral width'
    let height = fromIntegral height'
    processTurn ( x - (width`div`2), y - (height`div`2))

processTurn (x, y) = turnHorizontal x >> turnVertical y where -- есть даже какой-то специальный комбинатор типа split для этого
    turnHorizontal x = modifyIORef currentMatrix (!*! rotateAroundZ (fromGradi x))
    turnVertical y = modifyIORef currentAngle (\a -> bound (-tau/4) (tau/4) (a+fromGradi y))
    fromGradi x = (fromIntegral x / 360*tau)



--data Event a = Move (M44 a) | ToOrigin | ToggleCenter | Sanitize

redraw mesh = readIORef currentMatrix >>= G.display mesh

-- processEvent :: Event a -> IO ()
-- processEvent (Move a) = modifyIORef currentMatrix $ move a
-- processEvent ToOrigin = writeIORef currentMatrix identityIm



networkDescription :: forall a. (Floating a, Ord a, Show a, Real a) => Environment a -> (Int, Int) -> AddHandler Char -> AddHandler (Int, Int) -> MomentIO ()
networkDescription enviroment (width, height) addKeyboard addMouse = do
  ekeyboard <- fromAddHandler $ addKeyboard
  emouse' <- (fromAddHandler $ addMouse  )
  let mouseDelta = fmap (\(x,y) -> ( x - (width`div`2), y - (height`div`2))) emouse'
  let toGradi (x,y) = (ff x, ff y) where ff i = (fromIntegral i / 360*tau)
  let rotateDelta = fmap (\(p) -> rotateAroundZ p) (fmap fst $ toGradi <$> mouseDelta)
      ids = (identityIm)
      reset = fmap (const $ const ids) $ filterE (== 'r') ekeyboard

  rotate <- accumB startPosMatrix $ unions [(fmap (\x y -> x !*! y) rotateDelta),  reset]
  let moveDeltaRotated :: Event (M44 a)
      moveDeltaRotated = (filterJust $ fmap (flip lookup matrices) ekeyboard)
      straighten :: Behavior (M44 a -> M44 a)
      straighten = liftA2 (\x y z -> x !*! z !*! y) rotate (fmap invAroundZ rotate)

  let moveDelta = (straighten <@> moveDeltaRotated)
  (move::Behavior (M44 a)) <- accumB startPosMatrix (fmap (\x y -> pushOut (obstacles enviroment) (y !*! x)) $ moveDelta )
  -- let moveFunc::Event ((M44 a, M44 a, M44 a) -> (M44 a, M44 a, M44 a))
  --     moveFunc = fmap (\x (_, b, c) -> (x, b, c)) move
  -- let rotateFunc::Event ((M44 a, M44 a, M44 a) -> (M44 a, M44 a, M44 a))
  --     rotateFunc =  fmap (\x (a, _, c) -> (a, x, c)) $ rotate
  upAngle <- accumB 0 (fmap (\n delta -> bound (-tau/4) (tau/4) (n+delta)) (fmap snd $ toGradi <$> mouseDelta))
  let upMatrix = fmap rotateAroundY upAngle
      -- upFunc::Behavior ((M44 a, M44 a, M44 a) -> (M44 a, M44 a, M44 a))
      -- upFunc =  fmap (\x (a, b, _) -> (a, b, x)) upMatrix
      viewPortChangeStream = unionWith const (fmap (const ()) ekeyboard) (fmap (const ()) emouse')
      


  -- $(prettyR "upMatrix")
  -- $(prettyV "upAngle")
  -- let moveDeltaEvent = moveDelta <@ ekeyboard
  $(prettyR "moveDelta")
  
  let viewPortChange = liftA3 (\x y z -> moveAlongZ (-1/4) !*! x !*! y !*! z)  move rotate upMatrix  <@ viewPortChangeStream
  let dist = fmap (\x -> distance origin (x !$ origin)) viewPortChange -- <@ ekeyboard
  $(prettyV "dist")
  reactimate (fmap (\x -> display (mesh enviroment) (x)) viewPortChange)
  reactimate $ fmap (\x -> putStrLn $ "insanity:"++ show (insanity x) ++ "\n")  viewPortChange

bound low high x 
  | x < low = low
  | x < high = x
  | otherwise = high


matrices :: Floating a => [(Char, M44 a)]
matrices = [('w', moveAlongX (-0.1)), ('s', moveAlongX (0.1)), 
                ('a', moveAlongY (0.1)), ('d', moveAlongY (-0.1)),
                  ('z', moveAlongZ (0.01)), ('c', moveAlongZ (-0.01))]
