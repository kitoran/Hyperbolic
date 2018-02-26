{-# Language TemplateHaskell, ScopedTypeVariables, NoMonomorphismRestriction, DataKinds,DuplicateRecordFields, BangPatterns, LambdaCase #-}
module Main where

import Data.IORef(IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Debug.Trace
import Data.Time.Clock(UTCTime(UTCTime), getCurrentTime, diffUTCTime)
import Control.Monad(when)
import Control.Applicative(liftA2, liftA3)
import Data.List((++))
import qualified Data.Vector as V
import Codec.Wavefront hiding (Point, Triangle)
import qualified Codec.Wavefront
import GHC.Float 
import Text.Show.Pretty
import System.Environment
import System.IO.Error
-- import System.IO.SaferFileHandles я хотел использовать модные безопасные функции работы с файлами,
-- монадические регионы, все дела, но у меня файлхендлов вообще нет нигде, всё читается из файла одной функцией
import System.Console.Program
import System.Console.Argument
import System.Console.Command
import qualified Text.Read as TR
import  Control.Concurrent
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
import System.Exit 

bound :: Double -> Double -> Double -> Double
bound low high x 
  | x < low = low
  | x < high = x
  | otherwise = high

startState :: State
startState = State identity 0.1 0 (V3 0.0 0 0)

tick :: Double -> [RuntimeObstacle Double] -> State -> State
tick gravity level = (\s@(State pos height nod (V3 x y z)) -> if height > 8 then s {_height = 7.99, _speed = V3 x y (-z)} else s). pushOut (level){- . applyGravity gravity-} . applySpeed

matricesMoveInPlane :: Floating a => [(Char, a -> M33 a)]
matricesMoveInPlane = {-fmap (\(a, b) -> (a, b (1/cosh a))) -}[('w', moveAlongX3 ), ('s', moveAlongX3 . negate), 
                           ('a', moveAlongY3 ), ('d', moveAlongY3 . negate)]

runtimeObstacles :: [RuntimeObstacle Double]
runtimeObstacles = computeObs (obstacles level)
level  :: Environment (Double, Double, Double) Double
level = Env (Mesh [(red, Polygon [p0, p1, p2])]) ([Triangle p0 p1 p2 0.01])
  where p0 = Point 1.0 0.0 0.0 2.0
        p1 = rotateAroundZ (tau/3) !$ p0
        p2 = rotateAroundZ (-tau/3) !$ p0
        red = (1.0, 0.0, 0.0)
-- не ебу я как с трансформерами работать и вообще пишут в интернете что IO (Either x y) - это зло
loadObj' :: FilePath -> FilePath -> IO (Either String (Environment (Double, Double, Double) Double))
loadObj' pathMesh pathObs = do
  meshE <- fromFile pathMesh
  obsE <- fromFile pathObs
  return $ do
    meshObj <- meshE
    obsObj <- obsE
    mesh <- (parseMesh meshObj:: Either String (Mesh (Double, Double, Double) Double))
    obs <- parseObstacles obsObj
    return $ Env mesh obs

parseMesh :: WavefrontOBJ -> Either String (Mesh (Double, Double, Double) Double)
parseMesh obj = fmap Mesh $ mapM toPolygon $ map (elValue) $ V.toList $ objFaces obj
  where 
    toPolygon :: Face -> Either String ((Double, Double, Double), HyperEntity Double)
    toPolygon (Face i1 i2 i3 is) = do
      p1 <- toPoint i1
      p2 <- toPoint i2
      p3 <- toPoint i3
      ps <- mapM toPoint is
      Right $ ((0.5, 0.5, 0.5), Polygon ((p1): (p2): (p3):ps))
    toPoint :: FaceIndex -> Either String (Point Double)
    toPoint i = do
      (Location lx ly lz lw) <- maybe (Left $ "index out of range:" ++ ppShow i ++ "\n" ++ ppShow (objLocations obj)) Right (objLocations obj V.!? (faceLocIndex i-1))
      return $ Point (float2Double lx) (float2Double ly) (float2Double lz) (float2Double lw)

parseObstacles :: WavefrontOBJ -> Either String (Obstacles Double)
parseObstacles obj = mapM toTriangle $ map (elValue) $ V.toList $ objFaces obj
  where 
    toTriangle :: Face -> Either String (Obstacle Double)
    toTriangle (Codec.Wavefront.Triangle i1 i2 i3) = do
      p1 <- toPoint i1
      p2 <- toPoint i2
      p3 <- toPoint i3
      Right $ ( Triangle (p1) (p2) (p3) 0.01)
    toTriangle _ = Left "non-triangle face"
    toPoint :: FaceIndex -> Either String (Point Double)
    toPoint i = do
      (Location lx ly lz lw) <- maybe (Left $ "index out of range:" ++ ppShow i ++ "\n" ++ ppShow (objLocations obj)) Right (objLocations obj V.!? (faceLocIndex i-1))
      return $ Point (float2Double lx) (float2Double ly) (float2Double lz) (float2Double lw)

main :: IO ()
main = do 
        thId <- myThreadId 
        !meshRef <- newIORef (mesh level) 
        obsRef <- newIORef runtimeObstacles
        stepRef <- newIORef 0.01
        jumpRef <- newIORef 0.01
        gravityRef <- newIORef 0.0002
        frameRef <- newIORef True
        stateRef <- newIORef $ startState {_speed = V3 0.0 0 0.000}
        forkIO (glut thId obsRef meshRef stepRef jumpRef gravityRef frameRef stateRef)
        console obsRef meshRef stepRef jumpRef gravityRef frameRef stateRef
  where glut thId obsRef meshRef stepRef jumpRef gravityRef frameRef stateRef = 
          do
            initialiseGraphics
            GL.actionOnWindowClose $= GL.MainLoopReturns
            (Size width' height') <- get screenSize 
            let width = fromIntegral width'
            let height = fromIntegral height' 
            keyboardCallback $= (Just $ (\a _ -> do
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

            last <- newIORef $ UTCTime (toEnum 0) 1
            let display = do
                            state' <- readIORef stateRef
                            mesh <- readIORef meshRef
                            frame <- readIORef frameRef
                            displayGame (mesh :: Mesh (Double, Double, Double) Double) frame (viewPort state') 
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
            idleCallback $= (Just display)
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
        console obsRef meshRef stepRef jumpRef gravityRef frameRef stateRef =
          (interactive commands)
         where commands = (Node ( Command "" "" (io (return ())) True) [Node (setGravity gravityRef) [], 
                                                                         Node (loadLevel obsRef meshRef) [], 
                                                                         Node (loadObj obsRef meshRef) [], 
                                                                         Node (setStep stepRef) [], 
                                                                         Node (setJump jumpRef) [],
                                                                         Node (quit) [],
                                                                         Node (toggleFrame frameRef) [],
                                                                         Node (help commands) [],
                                                                         Node (state stateRef) []
                                                                        ])


toggleFrame :: IORef Bool -> Command IO
toggleFrame frameVar = command "toggleFrame" "toggleFrame toggles something" action 
    where
        action =  io (modifyIORef frameVar not)
                  

setGravity :: IORef Double -> Command IO
setGravity gravityVar = command "setGravity" "setGravity sets gravity" action 
    where
        action = withNonOption string (\s -> io (case TR.readEither s of
            Right num -> writeIORef gravityVar num
            Left error -> putStrLn error))

loadLevel :: IORef [RuntimeObstacle Double] -> IORef (Mesh (Double, Double, Double) Double) -> Command IO
loadLevel obsVar meshVar = command "load" "load <filename> loads enviromment from file <filename>" action 
    where
        action = withNonOption file (\path -> io $ do 
            enc <- catchIOError (fmap Right $ readFile path) (\_ -> putStrLn "error while opening file" >> return (Left ()))
            case enc of 
                Right enc' -> case TR.readEither enc' of 
                                Right (Env mesh slowObs) -> writeIORef meshVar mesh >> writeIORef obsVar (computeObs slowObs)
                                Left error -> putStrLn error
                Left () -> return ())

loadObj :: IORef [RuntimeObstacle Double] -> IORef (Mesh (Double, Double, Double) Double) -> Command IO
loadObj obsVar meshVar = command "loadObj" "loadObj <filename> loads enviromment from .obj file <filename>\n\
        \user can specify mesh file and osctacles files separately\n\
        \there shouldn't be any non-triangle faces in obstacle file" action 
  where
    action = withNonOption file (\path -> withNonOption (optional "" file)  
          (\pathObj -> io $ do
                    env <- loadObj' path (if pathObj==""then path else pathObj)
                    case env of 
                      Right (Env mesh slowObs) -> writeIORef meshVar mesh >> writeIORef obsVar (computeObs slowObs)
                      Left s -> putStrLn s)) 

setStep :: IORef Double -> Command IO
setStep stepVar = command "setStep" "setStep sets step length" action 
    where
        action = withNonOption string (\s -> io (writeIORef stepVar (read s)))


setJump :: IORef Double -> Command IO
setJump jumpVar = command "setJump" "setJump sets jump velocity" action 
    where
        action = withNonOption string (\s -> io (writeIORef jumpVar (read s)))

quit ::  Command IO
quit  = command "quit" "" action
    where 
        action = io (exitSuccess)

help :: Commands n -> Command IO 
help cnds = command "help" "shows this usage text" (io $ showUsage cnds)

window :: Command IO
window = command "window" "" action 
    where
        action = io (GL.leaveFullScreen)

fullscreen :: Command IO
fullscreen = command "fullscreen" "" action 
    where
        action = io (GL.fullScreen)

state :: IORef State -> Command IO
state stateVar = command "state" "query current position etc" action 
    where
        action =  io $ do
          s'@(State m _ _ _) <- readIORef stateVar
          print s'
          putStrLn $ "current xy position is " ++ show (m !* V3 0 0 1)

viewPort :: State -> M44 Double
viewPort (State pos height nod _) = rotateAroundY (-nod) !*! moveAlongZ (-height) !*! (m33_to_m44M $ transposeMink3 pos)

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



