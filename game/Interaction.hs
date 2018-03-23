{-# Language TemplateHaskell, ScopedTypeVariables, OverloadedStrings,
 NoMonomorphismRestriction, DataKinds,DuplicateRecordFields, 
 BangPatterns, LambdaCase,
 FlexibleContexts #-}
module Main where

import Data.IORef(IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Debug.Trace

import qualified Control.Monad.State as MTL
import Data.Time.Clock(UTCTime(UTCTime), getCurrentTime, diffUTCTime)
import Control.Monad(when, forM)
import Control.Applicative(liftA2, liftA3)
import Data.List((++))
import qualified Data.Vector as V
import Codec.Wavefront hiding (Point, Triangle)
import Console
import qualified Data.Tree               as T
import qualified Codec.Wavefront
import GHC.Float 
import Text.Show.Pretty
import System.Environment
import System.IO
import System.IO.Error
-- import System.IO.SaferFileHandles я хотел использовать модные безопасные функции работы с файлами,
-- монадические регионы, все дела, но у меня файлхендлов вообще нет нигде, всё читается из файла одной функцией
import qualified Text.Read as TR
import  Control.Concurrent
import System.Process
import qualified Graphics.UI.GLUT as GL
import Graphics.UI.GLUT
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
    ,    closeCallback
    ,    leaveMainLoop
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
-- import qualified Reactive.Banana.Frameworks(newAddHandler,
--                                   fromAddHandler, 
--                                   AddHandler, 
--                                   Handler, 
--                                   compile, 
--                                   actuate,
--                                   MomentIO,
--                                   reactimate)
-- import qualified Reactive.Banana.Combinators  
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

import Riemann 
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
-- import qualified Behaviour
import  Graphics as G (initialiseGraphics, displayGame, {-displayConsole-}) 
--import qualified Physics
import Physics
import Child
import System.Exit 

{-# INLINABLE (!?) #-}
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n
bound :: Double -> Double -> Double -> Double
bound low high x 
  | x < low = low
  | x < high = x
  | otherwise = high

startState :: State
startState = State identity 0.1 0 (V3 0.0 0 0)

viewPort :: State -> M44 Double
viewPort (State pos height nod _) = rotateAroundY (-nod) !*! moveAlongZ (-height) !*! (m33_to_m44M $ transposeMink3 pos)

tick :: Double -> [RuntimeObstacle Double] -> State -> State
tick gravity level = {-(\s@(State pos height nod (V3 x y z)) -> if height > 8 then s {_height = 7.99, _speed = V3 x y (-z)} else s).-} pushOut (level){- . applyGravity gravity-} . applySpeed

matricesMoveInPlane :: Floating a => [(Char, a -> M33 a)]
matricesMoveInPlane = {-fmap (\(a, b) -> (a, b (1/cosh a))) -}[('w', moveAlongX3 ), ('s', moveAlongX3 . negate), 
                           ('a', moveAlongY3 ), ('d', moveAlongY3 . negate)]

runtimeObstacles :: [RuntimeObstacle Double]
runtimeObstacles = computeObs (obstacles level)
level  :: Environment (Double, Double, Double) Double
level = Env (Mesh [(red, Polygon $ map (moveAlongX 1 !$) [p0, p1, p2])]) ([Triangle p0 p1 p2 0.01])
  where p0 = Point (1) 0.0 0.0 (1)
        p1 = rotateAroundZ (tau/3) !$ p0
        p2 = rotateAroundZ (-tau/3) !$ p0
        red = (1.0, 0.0, 0.0)
main :: IO ()
main = do 
        args <- getArgs
        if (args !? 0 == Just "console") then child else do
         thId <- myThreadId 
         !meshRef <- newIORef (mesh level) 
         obsRef <- newIORef runtimeObstacles
         stepRef <- newIORef 0.01
         jumpRef <- newIORef 0.01
         gravityRef <- newIORef 0.0002
         frameRef <- newIORef True
         wheConsoleRef <- newIORef False
         consoleRef <- newIORef (Console "" [])
         stateRef <- newIORef $ startState {_speed = V3 0.0 0 0.000}
        -- glut thId obsRef meshRef stepRef jumpRef gravityRef frameRef stateRef wheConsoleRef consoleRef
        -- console obsRef meshRef stepRef jumpRef gravityRef frameRef stateRef 
  -- where glut thId obsRef meshRef stepRef jumpRef gravityRef frameRef stateRef wheConsoleRef consoleRef = 
         do
            progPath <- getExecutablePath
            putStrLn "before"
            
            setEnv "TERM" "ansi"
            (inp,out,err,pid) <- runInteractiveProcess progPath ["console"] Nothing Nothing 
            -- hPutStr inp "str"
            hSetBuffering inp NoBuffering
            hSetBuffering out NoBuffering
            putStrLn "after"
            putStrLn "after-1"
            initialiseGraphics
            putStrLn "after initialiseGraphics"
            GL.actionOnWindowClose $= GL.MainLoopReturns
            (Size width' height') <- get screenSize 
            let width = fromIntegral width'
            let height = fromIntegral height' 
                
            keyboardCallback $= (Just $ (\a _ -> do
                               mod <- GL.getModifiers
                               if (a == '\t' && GL.ctrl mod == GL.Down) then (modifyIORef wheConsoleRef not) else do
                                wheCon <- readIORef wheConsoleRef
                                if wheCon then do
                                 hPutChar inp a 
                                 hFlush inp
                                 modifyIORef consoleRef (echo a)
                                  -- do
                                  -- cons <- readIORef consoleRef
                                  -- ((), newConsole) <- MTL.runStateT (interactive commands a) cons
                                  -- writeIORef consoleRef newConsole

                                          else do
                               -- when (not wheCon) $ do
                                
                                 step <- readIORef stepRef
                                 jump <- readIORef jumpRef
                                 modifyIORef stateRef $ processKeyboard step jump a)) 
                               -- \a _ -> case a of
                               -- 'q' -> leaveMainLoop 
                               -- 'c' -> do
                               --         displayCallback $= do
                               --                                                                                  state' <- readIORef state
                               --                                                                                  mesh <- readIORef meshRef
                               -- --                                                                                  displayGame (mesh) (viewPort state') 
                               --         modifyIORef consoleShown not
                               -- _   -> modifyIORef state $ processKeyboard a)
            -- GL.specialCallback $= (Just $ (\a _ -> do
                               -- when (a == GL.KeyUp) (modifyIORef consoleRef consoleUp)))
            
            last <- newIORef $ UTCTime (toEnum 0) 1
            let display =
                          do
                            state' <- readIORef stateRef
                            mesh <- readIORef meshRef
                            frame <- readIORef frameRef
                            cons <- readIORef consoleRef
                            wheCons <- readIORef wheConsoleRef
                            displayGame cons wheCons (mesh :: Mesh (Double, Double, Double) Double) frame (viewPort state') state'
            passiveMotionCallback $= (Just $ (\(Position x y) -> do
                last' <- readIORef last
                now <- getCurrentTime
                when (now `diffUTCTime ` last' > 0.03)
                    (do 
                     writeIORef last now 
                     modifyIORef stateRef $ processMouse width height (fromEnum x, fromEnum y)
                     pointerPosition $= (Position (width'`div`2) (height'`div`2))
                     display)
                ))
            displayCallback $= display
            escape <- newIORef (""::String) -- это должна быть mutable bytestring
            let  
                commands = (T.Node ( Command "" "" (io (return ())) True) [T.Node (setGravity gravityRef) [], 
                                                                           T.Node (loadLevel obsRef meshRef) [], 
                                                                           T.Node (loadObj obsRef meshRef) [], 
                                                                           T.Node (setStep stepRef) [], 
                                                                           T.Node (setJump jumpRef) [],
                                                                           T.Node (quit) [],
                                                                           T.Node (toggleFrame frameRef) [],
                                                                           T.Node (help commands) [],
                                                                           T.Node (Console.state stateRef) []
                                                                          ])
                pr = do
                 b <- hReady out
                 when(b) $ do
                   esc <- readIORef escape
                   c <- hGetChar out

                   putStrLn $ "pr: " ++ show esc ++ ", c: " ++ show c -- writeIORef escape ""
                   let news = esc ++ [c]
                   if isSequencePrefix (news) 
                     then
                      do 
                       putStrLn $ "in then, news = " ++ show news
                       case Console.sequence (news) of
                         Nothing -> do
                           putStrLn $ "in Nothing"
                       
                           writeIORef escape (news)
                         Just seqq-> do
                           putStrLn $ "in Just" ++ show seqq
                           modifyIORef consoleRef (applyEscapeSequence seqq) >> writeIORef escape ""
                     else 
                      -- error ("unknown seq: "++show news) 
                      do
                        putStrLn $ "in else" 
                        -- forM esc $ (\e -> do
                          -- cc <- readIORef consoleRef
                          -- newCC <- (MTL.execStateT (interactive commands e) cc)
                          -- writeIORef consoleRef newCC) -- (runStateTinteractive commands e))
                        -- 
                        writeIORef escape ""
                        cc <- readIORef consoleRef
                        newCC <- (MTL.execStateT (interactive commands c) cc)
                        writeIORef consoleRef newCC -- modifyIORefIO consoleRef (interactive commands c)
                        -- e <- readIORef consoleRef
                       -- ((), newConsole) <- MTL.runStateT (interactive commands c) cons
                       -- writeIORef consoleRef newConsole
                     -- str -> 
            idleCallback $= (Just (pr >> display))

            closeCallback $= Just (killThread thId)
            let timerCallback = do
                                    addTimerCallback 16 timerCallback
                                    gravity <- readIORef gravityRef
                                    obs <- readIORef obsRef
                                    modifyIORef stateRef $ tick gravity obs
        --                            readIORef state >>= (putStrLn . show)
            addTimerCallback 0 timerCallback
            pointerPosition $= (Position (width'`div`2) (height'`div`2))
            mainLoop
        -- console obsRef meshRef stepRef jumpRef gravityRef frameRef stateRef =
          -- (interactive commands)


processMove move = {-modifyIORef state-} (\(State pos height nod speed) -> State 
                                                                                                  (pos !*! move (0.1/cosh height) )
                                                                                                  height
                                                                                                  nod
                                                                                                  speed
                                                                                            )
matricesMoveZ = [('z', {-moveAlongZ-} (0.01)), ('c', {-moveAlongZ-} (-0.01))]


processKeyboard :: Double -> Double -> Char -> (State -> State)
processKeyboard step jump c = case lookup c matricesMoveInPlane of 
    Just m -> ((pos) %~ (!*! m step))
    _ -> case lookup c matricesMoveZ of 
            Just a -> (height %~ (+ a))
            Nothing -> case c of
                    'r' -> reset
                    ' ' -> (speed._z %~ (+ (jump)))
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
applyGravity :: Double -> State -> State
applyGravity gravity state@(State pos height nod cspeed@(V3 x y z)) = state { _speed = V3 x y (z - gravity/(cosh height)/(cosh height))}


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



