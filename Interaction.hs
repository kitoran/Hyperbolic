{-# Language TemplateHaskell, ScopedTypeVariables, NoMonomorphismRestriction, DataKinds,DuplicateRecordFields, BangPatterns, LambdaCase #-}
module Main where

import Data.IORef(IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Debug.Trace
import System.IO.Unsafe -- это так пока поиграться, потом уберу наверное
import Data.Time.Clock(UTCTime(UTCTime), getCurrentTime, diffUTCTime)
import Control.Monad(when)
import Control.Applicative(liftA2, liftA3)
import Data.List((++))
import System.Environment
import qualified Universe
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
    ,    idleCallback
    ,    mainLoop
    ,    addTimerCallback
    )
import Linear (
               (!*)
               , (*!)
               , (!*!)
               , M44
               , M33
               , inv44
               , V4(..)
               , V3(..)
               , V2 (..)
               , identity
               , _z
               , _x
               )
import Control.Lens-- (over)
-- import Reactive.Banana.Frameworks(newAddHandler,
--                                   fromAddHandler, 
--                                   AddHandler, 
--                                   Handler, 
--                                   compile, 
--                                   actuate,
--                                   MomentIO,
--                                   reactimate)
-- import Reactive.Banana.Combinators  
--     (
--       accumE
--     , Event
--     , filterJust
--     , unions
--     , filterE
-- --    , Behavior
--     , unionWith
--     , never
--     ) 

import Hyperbolic 
        -- ( rotateAroundZ
        -- , rotateAroundY
        -- , moveAlongZ
        -- , moveAlongX
        -- , moveAlongY
        -- , moveAlongX3
        -- , moveAlongY3
        -- , identityIm
        -- , insanity
        -- , pretty
        -- , invAroundZ
        -- , (!$)
        -- , origin
        -- , distance
        -- , commute
        -- , _v4
        -- , Point (..)
        -- , transposeMink
        -- , m33_to_m44M
        -- )
-- import Behaviour
import Graphics as G (initialiseGraphics, display) 
import DebugTH(prettyR, prettyV)
--import Physics
import Physics
import GHC.TypeLits
import System.Exit
--the unsafePerformIO hack выглядит понятнее гораздо, чем FRP. Можно передавать состояние, конечно, параметрами, но мы этого делать не будем (пока, может потом)
currentMatrix :: IORef (M44 Double)
currentMatrix = unsafePerformIO $ newIORef (identityIm)
currentAngle :: IORef Double
currentAngle = unsafePerformIO $ newIORef (0)

bound low high x 
  | x < low = low
  | x < high = x
  | otherwise = high

startState :: State
startState = State identity 0.1 0 (V3 0.0 0 0)

tick :: Environment (Double, Double, Double) Double -> State -> State
tick level = (\s@(State pos height nod (V3 x y z)) -> if height > 8 then s {_height = 7.99, _speed = V3 x y (-z)} else s). pushOut (obstacles level) . applyGravity . applySpeed

matricesMoveInPlane :: Floating a => [(Char, a -> M33 a)]
matricesMoveInPlane = {-fmap (\(a, b) -> (a, b (1/cosh a))) -}[('w', moveAlongX3 ), ('s', moveAlongX3 . negate), 
                           ('a', moveAlongY3 ), ('d', moveAlongY3 . negate)]

main :: IO ()
main = do

    !level <- return Universe.dodecahedronEnv --fmap read $ getArgs >>= (\case
     --   [] -> putStrLn "нужен аргумент - файл с уровнем" >> exitFailure 
     --   a:_ -> return a ) >>= readFile
    initialiseGraphics
    -- (addKeyboard, fireKeyboard) <- (newAddHandler::IO (AddHandler Char, Handler Char))
    -- (addMouse, fireMouse) <- (newAddHandler::IO (AddHandler (Int, Int), Handler (Int, Int)))
    -- (addDisplay, fireDisplay) <- (newAddHandler::IO (AddHandler (), Handler ()))
    -- (addTimer, fireTimer) <- (newAddHandler::IO (AddHandler (), Handler ()))
    last <- newIORef $ UTCTime (toEnum 0) 1
    (Size width' height') <- get screenSize 
    let width = fromIntegral width'
    let height = fromIntegral height' 
    state <- newIORef $ startState {_speed = V3 0.0 0 0.000}
    -- let network = networkDescription level (width, height) addKeyboard addMouse addDisplay addTimer
    -- compile network >>= actuate 
    keyboardCallback $= (Just $ \a _ -> modifyIORef state $ processKeyboard a)
    passiveMotionCallback $= (Just $ (\(Position x y) -> do
        last' <- readIORef last
        now <- getCurrentTime
        when (now `diffUTCTime ` last' > 0.04)
            (do 
             writeIORef last now 
             modifyIORef state $ processMouse width height (fromEnum x,fromEnum y)
             pointerPosition $= (Position (width'`div`2) (height'`div`2)))
        ))
    displayCallback $= (readIORef state >>= (display (mesh level) . viewPort))
    idleCallback $= (Just $ readIORef state >>= (display (mesh level) . viewPort))
    let timerCallback = do
                            addTimerCallback 5 timerCallback
                            modifyIORef state $ tick level
                            readIORef state >>= (putStrLn . show)
    addTimerCallback 0 timerCallback
    redraw (mesh level) 
    -- fireMouse (width`div`2, height`div`2)
    -- fireMouse (width`div`2, height`div`2)
    -- fireMouse (width`div`2, height`div`2)
    -- fireMouse (width`div`2, height`div`2)

    pointerPosition $= (Position (width'`div`2) (height'`div`2))
    mainLoop

viewPort :: State -> M44 Double
viewPort (State pos height nod _) = rotateAroundY (-nod) !*! moveAlongZ (-height) !*! (m33_to_m44M $ transposeMink3 pos)

processMove move = {-modifyIORef state-} (\(State pos height nod speed) -> State 
                                                                                                  (pos !*! move (0.1/cosh height) )
                                                                                                  height
                                                                                                  nod
                                                                                                  speed
                                                                                            )
matricesMoveZ = [('z', {-moveAlongZ-} (0.01)), ('c', {-moveAlongZ-} (-0.01))]


processKeyboard :: Char -> (State -> State)
processKeyboard c = case lookup c matricesMoveInPlane of 
    Just m -> ((pos) %~ (!*! m 0.01))
    _ -> case lookup c matricesMoveZ of 
            Just a -> (height %~ (+ a))
            Nothing -> case c of
                    'r' -> reset
                    ' ' -> (speed._z %~ (+ (0.01)))
                    'q' -> const Exceptionlol
                    _ -> id

processMouse :: Int -> Int -> (Int, Int) -> State -> State
processMouse width height (x, y) = 
    let 
        fromGradi x = (fromIntegral x / 360*tau)
    in processTurnUp ( fromGradi $ y - (height`div`2)) . processTurnLeft ( fromGradi $ x - (width`div`2) )


--   let toGradi (x,y) = (ff x, ff y) where ff i = (fromIntegral i / 360*tau)
--   let rotateDelta = fmap (\(p) -> rotate3 p) (fmap (fst) $ toGradi <$> mouseDelta)
applySpeed :: State -> State
applySpeed (State pos height nod speed@(V3 x y z)) = State (pos !*! ( moveToTangentVector3 (V2 x y)) ) (height+z) nod speed
applyGravity :: State -> State
applyGravity = id--state@(State pos height nod cspeed@(V3 x y z)) = state { _speed = V3 x y (z - 0.0001/(cosh height)/(cosh height))}

redraw mesh = readIORef currentMatrix >>= G.display mesh


-- processEvent :: Event a -> IO ()
-- processEvent (Move a) = modifyIORef currentMatrix $ move a
-- processEvent ToOrigin = writeIORef currentMatrix identityIm

--data Input = Reset | Move (Double -> M33 Double) | Down Double | Left Double | Tick 






--processEvent :: Input -> (State -> State)
reset state = startState
processTurnUp angle     = {-modifyIORef state-} (\(State pos height nod speed) -> State 
                                                                                                 pos
                                                                                                 height
                                                                                                 (bound (-tau/4) (tau/4) (nod + angle))
                                                                                                 speed
                                                                                            )
processTurnLeft angle      = {-modifyIORef state-} (\(State pos height nod speed) -> State 
                                                                                             (pos !*! (rotate3 (- angle)))
                                                                                             height
                                                                                             nod
                                                                                             ( speed ) -- Sudya po vsemu, skorost' nuzhno menyat' zdes' ili v processMove
                                                                                            )
-- networkDescription :: forall a. (RealFloat a, Ord a, Show a, Real a) => Environment a -> 
--                                                                         (Int, Int) -> 
--                                                                         AddHandler Char ->
--                                                                         AddHandler (Int, Int) ->
--                                                                         AddHandler () -> 
--                                                                         AddHandler () -> 
--                                                                         MomentIO ()
-- networkDescription environment (width, height) addKeyboard addMouse addDisplay addTimer = do
--   ekeyboard <- fromAddHandler $ addKeyboard
--   emouse' <- (fromAddHandler $ addMouse  )
--   edisplay <- (fromAddHandler $ addDisplay)
--   etimer <- fromAddHandler addTimer
--   let mouseDelta = fmap (\(x,y) -> ( x - (width`div`2), y - (height`div`2))) emouse'
--   let toGradi (x,y) = (ff x, ff y) where ff i = (fromIntegral i / 360*tau)
--   let rotateDelta = fmap (\(p) -> rotate3 p) (fmap (fst) $ toGradi <$> mouseDelta)
--       ids = (identityIm)
--   --let startPosMatrixM = V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)--(1))
--       resetRequest = filterE (== 'r') ekeyboard
--       reset = fmap (const $ const identityIm) $ resetRequest
--   -- let moveFunc::Event ((M44 a, M44 a, M44 a) -> (M44 a, M44 a, M44 a))
--   --     moveFunc = fmap (\x (_, b, c) -> (x, b, c)) move
--   -- let rotateFunc::Event ((M44 a, M44 a, M44 a) -> (M44 a, M44 a, M44 a))
--   --     rotateFunc =  fmap (\x (a, _, c) -> (a, x, c)) $ rotate
--   upAngle <- accumB 0 $ unions [(fmap (\n delta -> bound (-tau/4) (tau/4) (n+delta)) (fmap (negate.snd) $ toGradi <$> mouseDelta)), fmap (const $ const 0) $ resetRequest]
--   let upMatrix = fmap rotateAroundY upAngle
--   let --upMoveDelta :: Event (M44 Double)
--       upMoveDelta = (filterJust $ fmap (flip lookup matricesMoveZ) ekeyboard)
--   upMove <- accumB identityIm $ fmap (\x y -> x !*! y) upMoveDelta
--   curz <- fmap (\x -> distance origin (x !$ origin)) upMove <@ ekeyboard
--   let curzB :: Behaviour ((a -> M33 a) -> M33 a)
--       curzB = stepper ($ 0.1) (fmap (\a -> ($ 0.1/cosh a)) curz)
--   --let moveDelta :: Event (M44 a)
--   -- speed :: V33 a
--   speedE <- accumE (V3 0 0 0) $ unions [fmap (\_ (V3 x y z) -> (V3 0 0 0)) resetRequest, fmap (\_ (V3 x y z) -> V3 x y (z-0.0001)) etimer]
--   moveDeltaInteractive <- curzB <@> (filterJust $ fmap (\k -> lookup k matricesMoveInPlane ) ekeyboard)
--   (moveDelta::Event (M33 a)) <- unionWithP (!*!) identityIm identityIm moveDeltaInteractive never--(fmap (\(V3 _ _ z) -> (moveAlongZ (-z))) speedE)
--   (move::Behaviour (M33 a)) <- accumB identityIm $ unions [reset, (fmap (\x y -> x !*! y) moveDelta), fmap (\x y -> x !*! y) rotateDelta]--(move::Behaviour (M44 a)) <- accumB startPosMatrix $ unions [(fmap (\x y -> (y !*! x !*! transposeMink y)) $  unionWith (!*!) moveDeltaRotated rotateDelta ), reset]
--       -- upFunc::Behavior ((M44 a, M44 a, M44 a) -> (M44 a, M44 a, M44 a))
--       -- upFunc =  fmap (\x (a, b, _) -> (a, b, x)) upMatrix
--       -- viewPortChangeStream = unionWith const (fmap (const ()) ekeyboard) (fmap (const ()) emouse')


--   -- $(prettyR "upMatrix")
--   -- $(prettyV "upAngle")
--   -- let moveDeltaEvent = moveDelta <@ ekeyboard
--   -- $(prettyR "upMoveDelta")
--   -- $(prettyR "upMove")
  
--   let viewPort = liftA3 (\x y z -> {-moveAlongZ (-1/4) !*!-} z !*! y !*! x) (fmap m33_to_m44M move) upMove upMatrix  -- <@ viewPortChangeStream
--   idle <- viewPort <@ edisplay
--   -- let dist = fmap (\x -> distance origin (x !$ origin)) (idle)-- ::Event (M44 Double)) -- <@ ekeyboard
--   -- $(prettyV "dist")
--   -- let currp :: Event (Point a, Point a)
--       -- currp = fmap (\x -> ((x !$ origin), (over _v4  (*! x) origin))) (idle)-- ::Event (M44 Double)) -- <@ ekeyboard
--   -- $(prettyV "currp")
--   reactimate (fmap (\x -> display (mesh environment) (x)) idle)
--   -- reactimate $ fmap (\x -> putStrLn $ "insanity:"++ show (insanity x) ++ "\n")  idle



