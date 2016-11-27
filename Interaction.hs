{-# Language TemplateHaskell #-}
module Main where

import Data.IORef(newIORef, readIORef, writeIORef)
import Data.Time.Clock(UTCTime(UTCTime), getCurrentTime, diffUTCTime)
import Control.Monad(when)
import Control.Applicative(liftA2)
import Data.List((++))
import Graphics.UI.GLUT (Size(Size),
                         get, 
                         screenSize, 
                         ($=), 
                         Position(Position),
                         keyboardCallback,
                         passiveMotionCallback,
                         pointerPosition,
                         mainLoop)
import Linear ((!*!), M44, inv44)
import Reactive.Banana.Frameworks(newAddHandler,
                                  fromAddHandler, 
                                  AddHandler, 
                                  Handler, 
                                  compile, 
                                  actuate,
                                  MomentIO,
                                  reactimate)
import Reactive.Banana.Combinators (accumE, Event, filterJust, unions) 

import Behaviour(accumB, Behaviour, (<@>))
import Hyperbolic (rotateAroundZ, rotateAroundY, moveAlongZ, moveAlongX, moveAlongY, identityIm, insanity, pretty, invAroundZ, (!$), origin, distance)
import Graphics(initialiseGraphics, display)
import DebugTH(prettyR, prettyV)
--import Physics
import Universe (level, startPosMatrix, Environment(mesh), tau)

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
    keyboardCallback $= (Just $ \c p -> fireKeyboard (c::Char))
    passiveMotionCallback $= (Just $ (\(Position x y) -> do
       last' <- readIORef last
       now <- getCurrentTime
       when (now `diffUTCTime ` last' > 0.04)
            (do 
             writeIORef last now 
             fireMouse (fromEnum x,fromEnum y)
             pointerPosition $= (Position (width'`div`2) (height'`div`2)))))
    display (mesh level) startPosMatrix
    fireMouse (width`div`2, height`div`2)
    fireMouse (width`div`2, height`div`2)
    fireMouse (width`div`2, height`div`2)
    fireMouse (width`div`2, height`div`2)
    mainLoop

networkDescription :: Environment Double -> (Int, Int) -> AddHandler Char -> AddHandler (Int, Int) -> MomentIO ()
networkDescription enviroment (width, height) addKeyboard addMouse = do
  ekeyboard <- fromAddHandler $ addKeyboard
  emouse' <- (fromAddHandler $ addMouse  )
  let mouseDelta = fmap (\(x,y) -> ( x - (width`div`2), y - (height`div`2))) emouse'
  let toGradi (x,y) = (ff x, ff y) where ff i = (fromIntegral i / 360*tau)
  let rotateDelta = fmap (\(p) -> rotateAroundZ p) (fmap fst $ toGradi <$> mouseDelta)

  rotateB <- accumB startPosMatrix (fmap (\x y -> x !*! y) rotateDelta)
  rotate <- accumE startPosMatrix (fmap (\x y -> x !*! y) rotateDelta)
  let moveDeltaRotated :: Event (M44 Double)
      moveDeltaRotated = (filterJust $ fmap (flip lookup matrices) ekeyboard)
      straighten :: Behaviour (M44 Double -> M44 Double)
      straighten = liftA2 (\x y z -> x !*! z !*! y) rotateB (fmap invAroundZ rotateB)

  moveDelta <- straighten <@> moveDeltaRotated
  move <- accumE startPosMatrix (fmap (\x y -> y !*! x) $ moveDelta )
  let moveFunc::Event ((M44 Double, M44 Double, M44 Double) -> (M44 Double, M44 Double, M44 Double))
      moveFunc = fmap (\x (_, b, c) -> (x, b, c)) move
  let rotateFunc::Event ((M44 Double, M44 Double, M44 Double) -> (M44 Double, M44 Double, M44 Double))
      rotateFunc =  fmap (\x (a, _, c) -> (a, x, c)) $ rotate
  upAngle <- accumE 0 (fmap (\n delta -> bound (-tau/4) (tau/4) (n+delta)) (fmap snd $ toGradi <$> mouseDelta))
  let upMatrix = fmap rotateAroundY upAngle
      upFunc::Event ((M44 Double, M44 Double, M44 Double) -> (M44 Double, M44 Double, M44 Double))
      upFunc =  fmap (\x (a, b, _) -> (a, b, x)) upMatrix
  viewPortChangeStream <- accumE (identityIm, identityIm, identityIm) (unions [moveFunc, rotateFunc, upFunc])

  $(prettyR "upMatrix")
  $(prettyV "upAngle")
  $(prettyR "moveDelta")
  
  let viewPortChange = fmap (\(x,y,z) -> moveAlongZ (-1/4) !*! x !*! y !*! z)  viewPortChangeStream 
  let dist = fmap (\x -> distance origin (x !$ origin)) viewPortChange
  $(prettyV "dist")
  reactimate (fmap (\x -> display (mesh enviroment) (x)) viewPortChange)
  reactimate $ fmap (\x -> putStrLn $ "insanity:"++ show (insanity x) ++ "\n")  viewPortChange

bound low high x 
  | x < low = low
  | x < high = x
  | otherwise = high


matrices :: [(Char, M44 Double)]
matrices = [('w', moveAlongX (-0.1)), ('s', moveAlongX (0.1)), 
                ('a', moveAlongY (0.1)), ('d', moveAlongY (-0.1)),
                  ('z', moveAlongZ (0.01)), ('c', moveAlongZ (-0.01))]
