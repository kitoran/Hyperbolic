{-# Language NoMonomorphismRestriction, BangPatterns, RecursiveDo, TupleSections, TemplateHaskell #-}
module Main where
import Behaviour

import Control.Monad
import Control.Concurrent
import System.Exit
import Data.IORef
import Reactive.Banana hiding (stepper, (<@), (<@>), accumB)
import Reactive.Banana.Frameworks 
import Graphics.UI.GLUT hiding (Point)
--import State(stepForward)
import Universe
import DebugTH
import Linear hiding (perspective, lookAt, trace)
import Control.Lens
import Data.Time.Clock
import Debug.Trace

append = (++)
startPosMatrix = identityIm

both f (a, b) = (f a, f b)
--tau::Floating a => a
--tau = 2*pi 
--(-^), (+^) :: (Int, Int) -> (Int, Int) -> (Int, Int) 
(x, y) -^ ( z, w) = ((x-z) ,(y-w))
( x, y) +^ ( z, w) = ( (x+z) ,(y+w))
data MoveDirection = Fw | Bc | Lf | Rg deriving (Enum, Eq)
matrices :: [(Char, M44 Double)]
matrices = [('w', moveAlongX (-0.1)), ('s', moveAlongX (0.1)), 
                ('a', moveAlongY (0.1)), ('d', moveAlongY (-0.1)),
                  ('z', moveAlongZ (0.01)), ('c', moveAlongZ (-0.01))]

myPoints :: [(GLfloat,GLfloat, GLfloat)]
myPoints = [  (0, 1/2, 0), (0, 1/2, 3)]--(sin (2*pi*k/12), cos (2*pi*k/12)) | k <- [1..12] ]
upC :: Camera Double -> Camera Double
downC :: Camera Double -> Camera Double
upC = (\(Camera (Point x y z t) b c) -> Camera (Point (x+1) y z t) b c)
downC = (\(Camera (Point x y z t) b c) -> Camera (Point (x-1) y z t) b c)

bound low high x 
  | x < low = low
  | x < high = x
  | otherwise = high
main :: IO ()
main = do
   -- print enviroment
    --exitSuccess
 {-   print $ map (viewP cameraC) points { -}
    initialDisplayMode $= [ WithDepthBuffer, DoubleBuffered]
    (_progName, _args) <- getArgsAndInitialize

    _window <- createWindow "Hyperbolic"
    fullScreen

   -- polygonMode $= (Line, Fill)
    depthFunc $= Just Lequal
    depthBounds $= Nothing--Just (1/40,39/40)
    lineSmooth $= Enabled
    lineWidth $= 2
{-}
    matrixMode $= Projection
    perspective 40.0 1.0 1.0 10.0
    matrixMode $= Modelview 0
    lookAt (Vertex3 0.0 0.0 0.0) (Vertex3 1.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
  { -}
    (Size width' height') <- get screenSize 
    let width = fromIntegral width'
    let height = fromIntegral height'-- >>= (\(Size x y) -> (fromIntegral x, fromIntegral y))
    (addKeyboard, fireKeyboard) <- (newAddHandler::IO (AddHandler Char, Handler Char))
    (addMouse, fireMouse) <- (newAddHandler::IO (AddHandler (Int, Int), Handler (Int, Int)))
    listen <- newIORef True
    ct <- getCurrentTime
    last <- newIORef $ UTCTime (toEnum 0) 1
    (addDisplay, raiseDisplay) <- newAddHandler
    let segments = concat [[a, b] | Segment a b <- enviroment]  
    curmat <- newIORef (identity :: M44 Double)
    let enviroment = level-- <- parse <$> (readFile "level.dat")
    let networkDescription :: MomentIO ()
        networkDescription = do
           -- input: obtain  Event  from functions that register event handlers
          ekeyboard <- fromAddHandler $ addKeyboard
          emouse' <- (fromAddHandler $ addMouse  )
          ewithpr <- accumE ((0,0),(0,0)) ((\x (t, p) -> (p,x)) <$> emouse')
      --    emouse <- ( 0::Double, 0::Double) emouse'
          let mouseDelta = fmap (-^ (width`div`2, height`div`2)) emouse'
-- (\(x,y) -> y -^ x) <$> ewithpr--(\a f -> a -^ f) <$> mouseAbs <@> emouse'
--          mouseAbs <- accumB ( 0::Int, 0) ((\x y -> x +^ y) `fmap` mouseDelta)
          --let !move = (filterJust $ fmap (flip lookup matrices) ekeyboard)
          let toGradi (x,y) = (ff x, ff y) where ff i = (fromIntegral i / 360*tau)
          --let !rotate = fmap (\(a, p) -> rotateAroundZ a !*! rotateAroundY p) (toGradi <$> mouseDelta)
      --    let !viewPortDelta = unionWith (!*!) move rotate
          let  
          --        moveDeltaFunc = fmap (\x (_, y) -> (x, y)) moveDelta
              rotateDelta = fmap (\(p) -> rotateAroundZ p) (fmap fst $ toGradi <$> mouseDelta)


          rotateB <- accumB startPosMatrix (fmap (\x y -> x !*! y) rotateDelta)
          rotate <- accumE startPosMatrix (fmap (\x y -> x !*! y) rotateDelta)
          let moveDeltaRotated :: Event (M44 Double)
              moveDeltaRotated = (filterJust $ fmap (flip lookup matrices) ekeyboard)

--              moveDeltaB :: Behaviour (M44 Double)
--              moveDeltaB = liftA3 (\x y z -> z !*! y !*! x) rotateB moveRotated (fmap inv44 rotateB) <@ (filterJust $ fmap (flip lookup matrices) ekeyboard)
              straighten :: Behaviour (M44 Double -> M44 Double)
              straighten = liftA2 (\x y z -> x !*! z !*! y) rotateB (fmap inv44 rotateB)

          moveDelta <- straighten <@> moveDeltaRotated
            --      rotateDeltaFunc = fmap (\y (x, _) -> (x, y)) rotateDelta
      --        moveRotateStream <- accumE (identity, identity) (unionsC [moveDeltaFunc, rotateDeltaFunc])
          move <- accumE startPosMatrix (fmap (\x y -> y !*! x) $ moveDelta )
          let moveFunc::Event ((M44 Double, M44 Double, M44 Double) -> (M44 Double, M44 Double, M44 Double))
              moveFunc = fmap (\x (_, b, c) -> (x, b, c)) move
          --moveTrace <- valueB move

          -- rotate <- toEvent rotateB
          let rotateFunc::Event ((M44 Double, M44 Double, M44 Double) -> (M44 Double, M44 Double, M44 Double))
              rotateFunc =  fmap (\x (a, _, c) -> (a, x, c)) $ rotate
          upAngle <- accumE 0 (fmap (\n delta -> bound (-tau/4) (tau/4) (n+delta)) (fmap snd $ toGradi <$> mouseDelta))
          let upMatrix = fmap rotateAroundY upAngle
              upFunc::Event ((M44 Double, M44 Double, M44 Double) -> (M44 Double, M44 Double, M44 Double))
              upFunc =  fmap (\x (a, b, _) -> (a, b, x)) upMatrix
          viewPortChangeStream <- accumE (identityIm, identityIm, identityIm) (unions [moveFunc, rotateFunc, upFunc])
              --x !*!@ y = liftA2 (!*!) x y
          $(prettyR "upMatrix")
          $(prettyV "upAngle") 
          $(prettyR "moveDelta")

          let viewPortChange = fmap (\(x,y,z) -> moveAlongZ (-1/4) !*! x !*! y !*! z)  viewPortChangeStream --valueB $ stepper identity move !*!@ stepper identity rotate !*!@ stepper identity upMatrix 
          -- !viewPortChange <- accumE identityIm (fmap (\x y -> y !*! x) viewPortDelta)

          reactimate (fmap (\x -> {-do
            let movedEnviroment = (fmap) ( _ends._v4 %~ (*! x)) enviroment 
            let ts = [a| Segment w e  <- movedEnviroment,  a <- [w, e], (\(Point x _ _ _) -> x>0) a]
            let sp = [(y/x/width*600, z/x/height*600) | Point x y z t <- ts]
            writeIORef curmat sp-}
            display enviroment (x)
           {-  writeIORef listen FalseTrue-}) viewPortChange)
          reactimate $ fmap (\x -> putStrLn $ "insanity:"++ show (insanity x) ++ "\n" {-++
                                              pretty moveTrace-})  viewPortChange 
          let mo' = (stepper (0,1) emouse')
          mo <- (mo' <@ viewPortChangeStream )

          reactimate $ fmap (\x -> putStrLn $ "mo:"++ {-show (mo) ++ -}", emouse: " ++ show x ++ "\n" {-++
                                              pretty moveTrace-})   mo
          --reactimate $ fmap (\x -> putStrLn $ "\nmoveDelta:\n" ++
            --                                  pretty x)  moveDelta 
          -- $(prettyR "moveDelta")

    compile networkDescription >>= actuate
    cursor $= None
    displayCallback $= display enviroment identity
    keyboardCallback $= (Just $ \c p -> fireKeyboard (c::Char))
    passiveMotionCallback $= (Just $ (\(Position x y) -> do

       last' <- readIORef last
       now <- getCurrentTime
       when (now `diffUTCTime ` last' > 0.04)
            (do 
             writeIORef last now 
             fireMouse (fromEnum x,fromEnum y)
             pointerPosition $= (Position (width'`div`2) (height'`div`2))
             ) 
                                                       {->> 
                                                         pointerPosition $= (Position 0 0)-}) )
   {- idleCallback $= (Just $ do
                       Position x y <- get pointerPosition
                       fireMouse (toGradi x, toGradi y) 
                       pointerPosition $= (Position 0 0)) 
 --}
 --   display (identity::M44 Double) 
   {-} displayCallback $= (print "4")
     $= Just (\(Position x y) -> print $ "Mouse Callback!" ++ show x ++ ", "++ show y)

    idleCallback $= (Just (print "r" >> pointerPosition $= (Position 30 40)))-}
--    gameModeCapabilities $= [Where' GameModeWidth IsEqualTo 1024, Where' GameModeHeight IsEqualTo 600,
  --                            Where' GameModeRefreshRate IsEqualTo 60, 
    --                           Where' GameModeBitsPerPlane IsEqualTo 16]
    

    mainLoop
--    threadDelay 50000000  
    display enviroment identity
--    fireKeyboard 'w'
--    pointerPosition $= (Position 0 0)
--    pointerPosition $= (Position 0 6)
(asss, fffffg) = (1, 2)::(Int, Int)
colors = [(1, 0, 0), (1, 1, 0), (1, 1, 1), (1, 0, 1), (0, 0, 1), (0, 1, 0), (0, 1, 1), (0.5, 0.5, 1)]
level = Env $ zip colors [ HE f l u, ( HE f d l), HE f u r, HE f r d,
              HE b u l, HE b l d, HE b r u, HE b d r] 
  where f = Point (sinh w) 0 0 (cosh w)
        r = Point 0 (sinh w) 0 (cosh w)
        u = Point 0 0 (sinh w) (cosh w)
        b = Point (sinh (-w)) 0 0 (cosh w)
        l = Point 0 (sinh (-w)) 0 (cosh w)
        d = Point 0 0 (sinh (-w)) (cosh w)
        w = 1/3
{-level = Env $ zip colors [HE a b c, HE a b e]
          where a = Point 0 (-1)  0 1
                b = Point  0 1 0 1
                c = Point (sin (tau/12)) (cos (tau/12)) 0 1
                e = Point (sin (tau*5/12)) (cos (tau*5/12)) 0 1-}
toFrame::Enviroment Double -> [Point Double]
toFrame (Env []) = []
toFrame (Env ((_, HE x y z):xs)) = x:y:y:z:z:x:(toFrame (Env xs))
 
display :: Enviroment Double -> M44 Double -> DisplayCallback
display (Env env) tran = do
--  let s <- readIORef tran
  clear [ColorBuffer, DepthBuffer]
  color $ Color3 0 0 (0::GLfloat)
  renderPrimitive Triangles $ do 
     
    -- perspective 45 (1024/600) 0.1 20
     
    mapM_ toRaw env
--     mapM_ (\(x, y) -> vertex $ Vertex2 (x*0.9) (y*0.9)) $ concatMap (viewSegment cameraC) $  movedEnviroment
--    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  color $ Color3 1 1 (1::GLfloat)
  renderPrimitive Lines $ do
     mapM_ transform $ toFrame (Env env)
  swapBuffers
    where toRaw :: ((Double, Double, Double), HyperEntity Double) -> IO ()
          toRaw (c1, (HE a b c)) = do
                             -- color $ uncurry3 Color3 $ mapTuple coerce c1
                              transform a
                              transform b
                              transform c
          transform :: Point Double -> IO ()--Vertex4 Double
          transform p = let (V4 x y z t) = toV4 p *! tran in 
                     when (x>0) (vertex $ Vertex3 (y/x*600/1024) (z/x) (x/t)) 
          coerce :: Double -> GLfloat
          coerce  = fromRational.toRational
          uncurry3 f (a,b,c)=f a b c
          mapTuple f (a, b, c) = (f a, f  b, f c)
          --toRawLines :: Point Double -> IO ()
          --toRawLines 
--x 1
--(\Point x y z t -> vertex $ Vertex4 x y z t) $ concatMap (\He a b c -> [a, b, c])
{-}

display :: Enviroment Double -> M44 Double -> DisplayCallback
display (Env env) tran = do
--  let s <- readIORef tran
  clear [ColorBuffer, DepthBuffer]
  renderPrimitive Triangles $ do

    -- perspective 45 (1024/600) 0.1 20
     mapM_ toRaw env
--     mapM_ (\(x, y) -> vertex $ Vertex2 (x*0.9) (y*0.9)) $ concatMap (viewSegment cameraC) $  movedEnviroment
--    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  swapBuffers

    where toRaw :: HyperEntity Double -> IO ()
          toRaw (HE a b c) = do
                              transform a
                              transform b
                              transform c
          transform :: Point Double -> IO ()--Vertex2 Double
          transform p = let (V4 x y z t) =  toV4 p *! tran in
                              when (x>0)
                                   (vertex $ Vertex2 (y/x/1024*600) (z/x))-- x t
--(\Point x y z t -> vertex $ Vertex4 x y z t) $ concatMap (\He a b c -> [a, b, c])-}