{-# Language TemplateHaskell, ScopedTypeVariables, OverloadedStrings,
 NoMonomorphismRestriction, DataKinds,DuplicateRecordFields,
 BangPatterns, LambdaCase, NondecreasingIndentation,
 FlexibleContexts, MonoLocalBinds #-}
module Main where

import Data.IORef(IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Debug.Trace
import Prelude hiding (sinh)
import qualified Control.Monad.State as MTL
import Data.Time.Clock(UTCTime(UTCTime), getCurrentTime, diffUTCTime)
import Control.Monad(when, forM)
import Control.Applicative(liftA2, liftA3)
import Data.List((++))
import qualified Data.MonoTraversable as DM
import Data.Coerce
import Data.Monoid((<>))
import qualified Data.Vector as V
import Codec.Wavefront hiding (Point, Triangle)
import Console
import qualified Data.Tree               as T
import qualified Codec.Wavefront
--import GHC.Float hiding (sinh)
import Text.Show.Pretty
import System.Environment
import Data.Foldable
import System.IO
import System.IO.Error
-- import System.IO.SaferFileHandles я хотел использовать модные безопасные функции работы с файлами,
-- монадические регионы, все дела, но у меня файлхендлов вообще нет нигде, всё читается из файла одной функцией
import qualified Text.Read as TR
import  Control.Concurrent
-- import System.Process
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
import qualified Linear as L
import Hyperbolic as H
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
import qualified Graphics as G
--import qualified Physics
import Physics as P
-- import Child
import System.Exit
import Safe
import Data.Function

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
-- findSelected :: _
findSelected (WS de _) ap = foldMaybesP de ({-transposeMink- $-} G.viewPort ap)
 -- тут мы много раз считаем преобразование, а надо один если вообще считать

foldMaybesP :: [P.Deviator] -> M44 Double -> Maybe Int -- , H.Absolute Double)
foldMaybesP list ttt =   fmap snd $ minimumByMay (compare `on` fst) $ zip listt [0..]
  where listt :: [(Double)]
        listt = do
                 dev <- list
                 case intersectRay dev ttt of
                   Nothing -> []
                   Just (dis) -> 
                     return (dis{-, ((P._devPos) dev, diir)-}) --]filter (map (\e@(P.Devi poos _ _) -> ) list)

intersectRay :: Deviator -> M44 Double -> Maybe Double
intersectRay (P.Devi pos dir nod) transs = if any (\(_, Polygon a) ->  G.containsZero (map mapping a)) $ 
                                                      {-  traceShow (map (\(_, Polygon a) -> (map mapping a) ) list)-} list 
                          then Just $ distance (pos) origin
                          else Nothing
--if trace ("newdir = "++show newDir) cond then Just (x/t, newDir) else Nothing
  where
    Mesh list = {-transposeMink-} trans !$ G.deviator
    trans = let move = moveRightTo pos
                dirFromStart = (toNonPhysicalPoint $ transposeMink move !$ dir)
                turn = (getPointToOxyAroundOx `andThen`  getPointToOxzAroundOz) dirFromStart
             in (move !*! turn) {-}
             -- ((moveRightTo pos `andConsideringThat`  
                                         -- (getPointToOxyAroundOx `andThen`  getPointToOxzAroundOz))
                                         -- (toNonPhysicalPoint dir))-}
    mapping p = case transs !$ p of
                  (Point x y z t) -> {-Debug.Trace.trace ("What intersectRay sees: " <> show transs){ -} L.V2 (y/t) (z/t)
    -- res@(H.Point x y z t) = trans !$ dpos
    -- newDir = {-H.moveAlongY (0.1) !$ H.rotateAroundZ (H.tau/4) !$ ddir -} transposeMink trans !*! rotateAroundX (-d) !*! H.moveAlongX (H.distance H.origin res) !$ (H.Abs 0 1 0) -- } trans !$ (H.Abs 0 1 0)
    -- cond = abs y < 0.01 && abs z < 0.01 && x*t > 0 -- и ещё условие что девиатор правильно повёрнут
-- вызывать гиперболический косинус от гиперболического арккосинуса очень весело

startState :: LevelState
startState = LS (AP identity 0.1 0 (V3 0.0 0 0)) (Nothing) (WS [moveAlongY (0.1::Double) !$ Devi (Point 0 0 0 1) (Abs 0 1 0) (0) 
                                                               {- moveAlongY (-0.1::Double) !$ Devi (Point 0 0 0 1) (Abs 0 1 0) (0) -}  ] []) (Just 0)
-- { _avatarPosition :: AvatarPosition,
--                        _avatarInventory :: Item,
--                        _worldState :: WorldState
--                      }
thisFunc :: M44 Double -> (P.LevelState -> P.LevelState)
thisFunc trans (LS (ap@(AP mat hei nod _)) (Just De) (WS des dis) _) = let nmat = m33_to_m44M mat in LS ap Nothing (WS (Devi ({-nmat !*! moveAlongZ hei-}trans !$ (Point 0 0 0 1)) ({-nmat-} trans !$ Abs (1) 0 0 ) 0: des) (dis)) Nothing
thisFunc _ s@(LS (ap@(AP mat hei nod _)) (Nothing) (WS des dis) e) = case e of
                                                                    Nothing -> s
                                                                    Just i -> (LS (ap) (Just De) (WS [ x | (ix, x) <- zip [0..] des, ix /= i ] dis) e) -- = LS (ap@(mat hei nod)) (Nothing) (WS des dis) = LS ap Nothing (WS (Devi (mat !*! moveAlongZ hei !$ (Point 0 0 0 1)): des) (dis))

preShow :: Mesh -> M44 Double -> M44 Double
preShow rays vp = case rays of 
    Mesh [] -> transposeMink (vp) <> H.moveAlongX 0.011 <> H.moveAlongZ (-0.012) 
    _ -> trans 
  where
        ((_, ratio), index) = minimumBy (compare `on` (fst.fst)) $ zip (map (\(_::(Double, Double, Double, Double), s) -> sqDistanceFromProj s vp) $ coerce rays) [0..]
        (_::(Double, Double, Double, Double), P.Segment pos dir) = (coerce rays :: [((Double, Double, Double, Double), HyperEntity)]) !! index 
        rayToOx = let move = moveRightTo pos -- если сделать, чтобы одна функция возвращала moveRightTo и moveRightFrom, то меньше вычислений
                      dirFromStart = (transposeMink move !$ dir)
                      turn = (getPointToOxyAroundOy `andThen`  getPointToOxzAroundOz) dirFromStart
                   in (move <> transposeMink turn)
        Point dx dy dz dt = dir
        Point px py pz pt = pos
        trans = Debug.Trace.trace ("qqqq "++show x1) $ ( rayToOx) <> (moveAlongX (   (distance ( vp !$ pos) p)))
        (p::Point Double) = ((\a b -> (a)*(1-ratio)+b*( ratio)) <$> normalizeKlein (vp !$ pos) <*> normalizeKlein (vp !$ dir)::Point Double)
        (Point xh1 yh1 zh1 (th1::Double)) = vp !$ p
        (x1::Double) = ((negate yh1)/xh1)
        
        -- 
traceComm s a = Debug.Trace.trace (s ++ " " ++ show a) a
sqDistanceFromProj :: HyperEntity -> M44 Double -> (Double, {-lazy-}Double)
sqDistanceFromProj (P.Segment a b) trans = (dis, rr)
  where (L.V2 x1 y1) = L.V2 ((-yh1)/xh1) (zh1/xh1)
        (L.V2 x2 y2) = L.V2 ((-yh2)/xh2) (zh2/xh2)
        (Point xh1 yh1 zh1 th1) = trans !$ a 
        (Point xh2 yh2 zh2 th2) = trans !$ b

        numeratorRoot = (x2)*y1 - y2*(x1)
        denominator = (y2-y1)*(y2-y1) + ((traceComm "x2" x2)-(traceComm "x1" x1))*(x2-x1)
        dis = numeratorRoot*numeratorRoot / denominator
        ratio = traceComm "ratio" $ (x1*(x1-x2) + y1*(y1-y2) )/denominator
        nx = traceComm "nx" $ x1 + ratio*(x2-x1)
        rr = (nx*xh1 - yh1)/((-yh1)+yh2*(th1/th2)+nx*xh1-nx*xh2*(th1/th2))
-- castedRay = transpose trans !$ Ray (0 0 0 1) neare
-- castedRay = transpose trans !$ Point (1 nx ny t)
-- (t nx*t ny*t 1) = (1-a)*(x1 y1 z1 t1) + a*(x2 y2 z2 t2) where everthing is normalizedKlein
-- t = (1-a)*x1+a*x2
-- nx*t = (1-a)*y1+a*y2
-- (1-a)*y1+a*y2 = nx*((1-a)*x1+a*x2)
-- y1 + a*(-y1+y2) = nx*x1-nx*a*x1+nx*a*x2
-- a*(-y1+y2+nx*x1-nx*x2) = nx*x1 - y1
-- a = (nx*x1 - y1)/(-y1+y2+nx*x1-nx*x2)
-- если восстановить t: 
-- a = (nx*x1 - y1)/(-y1+y2(t1/t2)+nx*x1-nx*x2(t1/t2))
tick :: Double -> [RuntimeObstacle ] -> AvatarPosition -> AvatarPosition
tick gravity level = (\s@(AP pos height nod (V3 x y z)) -> if height > 8 then s {_height = 7.99, _speed = V3 x y (-z)} else s). pushOut (level) . applyGravity gravity . applySpeed

matricesMoveInPlane :: Floating a => [(Char, a -> M33 a)]
matricesMoveInPlane = {-fmap (\(a, b) -> (a, b (1/cosh a))) -}[('w', moveAlongX3 ), ('s', moveAlongX3 . negate),
                           ('a', moveAlongY3 ), ('d', moveAlongY3 . negate)]

runtimeObstacles :: [RuntimeObstacle ]
runtimeObstacles = computeObs (obstacles level)
level :: Environment  
level = Env (Mesh [(red, Polygon [p0, p1, p2])]) 
            ([Triangle p0 p1 p2 0.01]) 
            [Source (Point 0 (-0.001) 0.0001 2) (Abs 1.0001 0.00009999 (-0.0001))] 
            [Receiver l1, Receiver l2]
  where p0' = Point (sinh 1) 0.0 0.0 (cosh 1)
        p1' = rotateAroundZ (tau/3::Double) !$ p0'
        p2' = rotateAroundZ (-tau/3::Double) !$ p0'
        [p0, p1, p2] = map (moveAlongZ (-0.1::Double) !$) [p0', p1', p2']
        r0' = Point 0 0.1 0.1 2
        r1' = rotateAroundX (tau/4::Double) !$ r0'
        r2' = rotateAroundX (tau/4::Double) !$ r1'
        r3' = rotateAroundX (tau/4::Double) !$ r2'
        l1 = map (moveAlongX (0.1::Double) !$) [r0', r1', r2', r3']
        l2 = map (moveAlongX (0.2::Double) !$) [r0', r3', r2', r1']
        red = (1.0, 0.0, 0.0, 1)

main :: IO ()
main = do
         -- args <- getArgs
        -- if (args !? 0 == Just "console") then child else do
         thId <- myThreadId
         !levelMeshRef <- newIORef (mesh level)
         obsRef <- newIORef runtimeObstacles
         sourceRef <- newIORef (sources level)
         stepRef <- newIORef 0.01
         jumpRef <- newIORef 0.01
         gravityRef <- newIORef 0.0002
         mutableMeshRef <- newIORef (G.toMesh (sources level) (receivers level) startState)
         frameRef <- newIORef True
         wheConsoleRef <- newIORef False
         consoleRef <- newIORef (Console "" [] 0)
         stateRef <- newIORef $ startState 
         receiversRef <- newIORef (receivers level)
        -- glut thId obsRef meshRef stepRef jumpRef gravityRef frameRef stateRef wheConsoleRef consoleRef
        -- console obsRef meshRef stepRef jumpRef gravityRef frameRef stateRef
  -- where glut thId obsRef meshRef stepRef jumpRef gravityRef frameRef stateRef wheConsoleRef consoleRef =
         do
            G.initialiseGraphics
            -- GL.actionOnWindowClose $= GL.MainLoopReturns
            (Size width' height') <- get screenSize
            let width = fromIntegral width'
            let height = fromIntegral height'

            let
                commands = (T.Node ( Command "" "" (io (return ())) True) [T.Node (setGravity gravityRef) [],
                                                                           T.Node (loadLevel obsRef levelMeshRef sourceRef receiversRef) [],
                                                                           T.Node (loadObj obsRef levelMeshRef) [],
                                                                           T.Node (setStep stepRef) [],
                                                                           T.Node (setJump jumpRef) [],
                                                                           T.Node (quit) [],
                                                                           T.Node (toggleFrame frameRef) [],
                                                                           T.Node (help commands) [],
                                                                           T.Node (Console.state stateRef) []
                                                                          ])
            keyboardCallback $= (Just $ (\a _ -> do
                               mod <- GL.getModifiers
                               if (a == '\t' && GL.ctrl mod == GL.Down) then (modifyIORef wheConsoleRef not) else do
                                wheCon <- readIORef wheConsoleRef
                                if wheCon then do
                                 -- let act = if (a == '\r') then '\n' else if (a == '\b') then '\b' else a
                                 -- hPutChar inp  act
                                 -- hFlush inp
                                 -- modifyIORef consoleRef (echo a)
                                 when (a == '\EOT' || a == '\ETX') $ exitSuccess
                                  -- do
                                 (cons::Console) <- readIORef consoleRef
                                 newConsole <- MTL.execStateT (interactive commands a) cons
                                 writeIORef consoleRef newConsole

                                          else do
                               -- when (not wheCon) $ do

                                 step <- readIORef stepRef
                                 jump <- readIORef jumpRef
                                 modifyIORef stateRef (processKeyboard step jump a)))
                               -- \a _ -> case a of
                               -- 'q' -> leaveMainLoop
                               -- 'c' -> do
                               --         displayCallback $= do
                               --                                                                                  state' <- readIORef state
                               --                                                                                  mesh <- readIORef meshRef
                               -- --                                                                                  displayGame (mesh) (viewPort state')
                               --         modifyIORef consoleShown not
                               -- _   -> modifyIORef state $ processKeyboard a)
            GL.specialCallback $= (Just $ (\a _ -> do
                               when (a == GL.KeyUp) (modifyIORef consoleRef consoleUp)))

            last <- newIORef $ UTCTime (toEnum 0) 1
            let display =
                          do
                            state' <- readIORef stateRef
                            levelMesh <- readIORef levelMeshRef
                            (rays, itemss, recvs) <- readIORef mutableMeshRef
                            cons <- readIORef consoleRef
                            frame <- readIORef frameRef
                            sources <- readIORef sourceRef
                            wheCons <- readIORef wheConsoleRef
                            let ap = _avatarPosition state'
                                inv = case _avatarInventory state' of
                                       Nothing -> mempty
                                       Just De -> preShow rays (G.viewPort ap) !$ DM.omap (\((r,g,b,_),e) -> ((r, g, b, 0.5::Double), e)) G.deviator  --
                            let items = case (_selected state') of
                                         Nothing -> itemss
                                         Just i -> itemss & ix i %~ (\(Mesh a) -> Mesh (fmap (\((q, w, e, t), r) -> ((f q, f w, f e, t), r)) a))
                                  where f a = if a >= (1/3) then 1 else a+2/3 
                            G.displayGame cons wheCons (levelMesh <> fold items <> rays <> inv <> recvs) frame (G.viewPort $ ap) (ap)
            passiveMotionCallback $= (Just $ (\(Position x y) -> do
                last' <- readIORef last
                now <- getCurrentTime
                when (now `diffUTCTime ` last' > 0.03)
                    (do
                     writeIORef last now
                     LS vatarPosition vatarInventory orldState _  <- readIORef stateRef
                     let newap = processMouse width height (fromEnum x, fromEnum y) vatarPosition
                         newLected = case vatarInventory of
                                       Nothing -> findSelected orldState newap --vatarPosition
                                       _ -> Nothing
                     let neww = (LS newap vatarInventory orldState newLected)
                     writeIORef stateRef  neww
                     -- modifyIORef stateRef $ ((avatarPosition %~ ) . (selected %~ p))
                     pointerPosition $= (neww `seq` (Position (newap `seq`
                                                               newLected `seq` 
                                                               (width'`div`2)) 
                                                              (height'`div`2)))
                     -- display
                     )
                ))
            GL.mouseCallback $= Just (\GL.LeftButton butt _ -> case butt of
                                                                GL.Down -> do 
                                                                              ap <- readIORef stateRef
                                                                              (rays, _, _) <- readIORef mutableMeshRef
                                                                              let trans = preShow rays (G.viewPort $ _avatarPosition ap)
                                                                              writeIORef stateRef (thisFunc trans ap)
                                                                              so <- readIORef sourceRef
                                                                              st <- readIORef stateRef
                                                                              re <- readIORef receiversRef
                                                                              writeIORef mutableMeshRef (G.toMesh (so) re st)
                                                                GL.Up -> return ())
            displayCallback $= display
            escape <- newIORef (""::String) -- это должна быть mutable bytestring
                -- pr = do
                --  b <- hReady out
                --  when(b) $ do
                --    esc <- readIORef escape
                --    c <- hGetChar out

                --    putStrLn $ "pr: " ++ show esc ++ ", c: " ++ show c -- writeIORef escape ""
                --    let news = esc ++ [c]
                --    if isSequencePrefix (news)
                --      then
                --       do
                --        putStrLn $ "in then, news = " ++ show news
                --        case Console.sequence (news) of
                --          Nothing -> do
                --            putStrLn $ "in Nothing"

                --            writeIORef escape (news)
                --          Just seqq-> do
                --            putStrLn $ "in Just" ++ show seqq
                --            modifyIORef consoleRef (applyEscapeSequence seqq) >> writeIORef escape ""
                --      else
                --       -- error ("unknown seq: "++show news)
                --       do
                --         when (esc /= "") $ error "unknown esc seq"
                --         putStrLn $ "in else"
                --         -- forM esc $ (\e -> do
                --           -- cc <- readIORef consoleRef
                --           -- newCC <- (MTL.execStateT (interactive commands e) cc)
                --           -- writeIORef consoleRef newCC) -- (runStateTinteractive commands e))
                --         --
                --         writeIORef escape ""
                --         cc <- readIORef consoleRef
                --         newCC <- (MTL.execStateT (interactive commands c) cc)
                --         writeIORef consoleRef newCC -- modifyIORefIO consoleRef (interactive commands c)
                --    pr     -- e <- readIORef consoleRef
                       -- ((), newConsole) <- MTL.runStateT (interactive commands c) cons
                       -- writeIORef consoleRef newConsole
                     -- str ->
            idleCallback $= (Just ( display))

            GL.actionOnWindowClose $=  (GL.Exit)
            let timerCallback = do
                                    addTimerCallback 16 timerCallback
                                    gravity <- readIORef gravityRef
                                    obs <- readIORef obsRef
                                    modifyIORef stateRef (avatarPosition %~ tick gravity obs)
        --                            readIORef state >>= (putStrLn . show)
            addTimerCallback 0 timerCallback
            pointerPosition $= (Position (width'`div`2) (height'`div`2))
            mainLoop
        -- console obsRef meshRef stepRef jumpRef gravityRef frameRef stateRef =
          -- (interactive commands)


processMove move = {-modifyIORef state-} (\(AP pos height nod speed) -> AP
                                                                                                  (pos !*! move (0.1/cosh height) )
                                                                                                  height
                                                                                                  nod
                                                                                                  speed
                                                                                            )
matricesMoveZ = [('z', {-moveAlongZ-} (0.01)), ('c', {-moveAlongZ-} (-0.01))]


processKeyboard :: Double -> Double -> Char -> (LevelState -> LevelState)
processKeyboard step jump c = case lookup c matricesMoveInPlane of
    Just m -> ((avatarPosition.pos) %~ (!*! m step))
    _ -> case lookup c matricesMoveZ of
            Just a -> (avatarPosition.height %~ (+ a))
            Nothing -> case c of
                    'r' -> reset
                    ' ' -> (avatarPosition.speed._z %~ (+ (jump)))
                    _ -> id

processMouse :: Int -> Int -> (Int, Int) -> AvatarPosition -> AvatarPosition
processMouse width height (x, y) =
    let
        fromGradi x = (fromIntegral x / 360*tau*7.0/30.0)
    in processTurnUp ( fromGradi $ y - (height`div`2)) . processTurnLeft ( fromGradi $ x - (width`div`2) )


--   let toGradi (x,y) = (ff x, ff y) where ff i = (fromIntegral i / 360*tau)
--   let rotateDelta = fmap (\(p) -> rotate3 p) (fmap (fst) $ toGradi <$> mouseDelta)
applySpeed :: AvatarPosition -> AvatarPosition
applySpeed (AP pos height nod speed@(V3 x y z)) = AP (pos !*! ( moveToTangentVector3 (V2 x y)) ) (height+z) nod speed
applyGravity :: Double -> AvatarPosition -> AvatarPosition
applyGravity gravity state@(AP pos height nod cspeed@(V3 x y z)) = state { _speed = V3 x y (z - gravity/(cosh height)/(cosh height))}


-- processEvent :: Event a -> IO ()
-- processEvent (Move a) = modifyIORef currentMatrix $ move a
-- processEvent ToOrigin = writeIORef currentMatrix identityIm

--data Input = Reset | Move (Double -> M33 Double) | Down Double | Left Double | Tick






--processEvent :: Input -> (AvatarPosition -> AvatarPosition)
reset state = startState
processTurnUp angle     = {-modifyIORef state-} (\(AP pos height nod speed) -> AP
                                                                                                 pos
                                                                                                 height
                                                                                                 (bound (-tau/4) (tau/4) (nod + angle))
                                                                                                 speed
                                                                                            )
processTurnLeft angle      = {-modifyIORef AP-} (\(AP pos height nod speed) -> AP
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



