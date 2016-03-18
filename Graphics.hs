{-# Language NoMonomorphismRestriction, BangPatterns, RecursiveDo #-}
module Main where
import Control.Monad
import Data.IORef
import Reactive.Banana 
import Reactive.Banana.Frameworks 
import Graphics.UI.GLUT hiding (Point)
--import State(stepForward)
import Universe
import Linear
import Control.Lens
import Data.Time.Clock

--tau::Floating a => a
--tau = 2*pi 
(-^), (+^) :: (Int, Int) -> (Int, Int) -> (Int, Int) 
(x, y) -^ ( z, w) = ((x-z) ,(y-w))
( x, y) +^ ( z, w) = ( (x+z) ,(y+w))
data MoveDirection = Fw | Bc | Lf | Rg deriving (Enum, Eq)
matrices :: [(Char, M44 Double)]
matrices = [('w', moveAlongX (-0.1)), ('s', moveAlongX (0.1)), 
                ('a', moveAlongY (0.1)), ('d', moveAlongY (-0.1))]

myPoints :: [(GLfloat,GLfloat, GLfloat)]
myPoints = [  (0, 1/2, 0), (0, 1/2, 3)]--(sin (2*pi*k/12), cos (2*pi*k/12)) | k <- [1..12] ]
upC :: Camera Double -> Camera Double
downC :: Camera Double -> Camera Double
upC = (\(Camera (Point x y z t) b c) -> Camera (Point (x+1) y z t) b c)
downC = (\(Camera (Point x y z t) b c) -> Camera (Point (x-1) y z t) b c)
main :: IO ()
main = do
 {-   print $ map (viewP cameraC) points { -}
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Hello World"
    
    (addKeyboard, fireKeyboard) <- (newAddHandler::IO (AddHandler Char, Handler Char))
    (addMouse, fireMouse) <- (newAddHandler::IO (AddHandler (Int, Int), Handler (Int, Int)))
    listen <- newIORef True
    ct <- getCurrentTime
    last <- newIORef ct
    (addDisplay, raiseDisplay) <- newAddHandler
    curmat <- newIORef []
    let networkDescription :: MomentIO ()
        networkDescription = mdo
           -- input: obtain  Event  from functions that register event handlers
          ekeyboard <- fromAddHandler $ addKeyboard
          emouse' <- (fromAddHandler $ addMouse  )
          ewithpr <- accumE ((0,0),(0,0)) ((\x (t, p) -> (p,x)) <$> emouse')
      --    emouse <- ( 0::Double, 0::Double) emouse'
          let mouseDelta = (\(x,y) -> y -^ x) <$> ewithpr--(\a f -> a -^ f) <$> mouseAbs <@> emouse'
          mouseAbs <- accumB ( 0::Int, 0) ((\x y -> x +^ y) `fmap` mouseDelta)
          let !move = (filterJust $ fmap (flip lookup matrices) ekeyboard)
          let toGradi (x,y) = (ff x, ff y) where ff i = (fromIntegral i / 360*tau)
          let !rotate = fmap (\(a, p) -> rotateAroundZ a !*! rotateAroundY p) (toGradi <$> mouseDelta)
          let !viewPortDelta = unionWith ((!*!)) move rotate
          !viewPortChange <- accumE identity (fmap (\x y -> y !*! x) viewPortDelta)
          reactimate (fmap (\x -> do
            let movedEnviroment = (fmap) ( _ends._v4 %~ (*! x)) enviroment 
            let ts = [a| Segment w e  <- movedEnviroment,  a <- [w, e], (\(Point x _ _ _) -> x>0) a]
            let sp = [(y/x, z/x) | Point x y z t <- ts]
            writeIORef curmat sp
            display curmat listen
           {-  writeIORef listen FalseTrue-}) viewPortChange)
          reactimate (print <$> mouseDelta)

    compile networkDescription >>= actuate
    displayCallback $= display curmat listen
    keyboardCallback $= (Just $ \c p -> fireKeyboard (c::Char))
    passiveMotionCallback $= (Just $ (\(Position x y) -> do
                                                         last' <- readIORef last
                                                         now <- getCurrentTime
                                                         when (now `diffUTCTime ` last' > 0.04)
                                                              (writeIORef last now >>
                                                               fireMouse (fromEnum x,fromEnum y)) 
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
    mainLoop
    display curmat listen
    pointerPosition $= (Position 0 0)
    pointerPosition $= (Position 0 6)
    
    
 
display :: IORef [(Double, Double)]-> IORef Bool -> DisplayCallback
display tran lis = do
  s <- readIORef tran
  --let s = cam
  clear [ColorBuffer]
  renderPrimitive LineLoop $
     mapM_ (\(x, y) -> vertex $ Vertex2 (x*0.9) (y*0.9)) s
--     mapM_ (\(x, y) -> vertex $ Vertex2 (x*0.9) (y*0.9)) $ concatMap (viewSegment cameraC) $  movedEnviroment
--    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  flush
  lis $= True