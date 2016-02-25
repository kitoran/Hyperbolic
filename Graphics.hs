module Main where
{-import Graphics.UI.GLUT
 
myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]
 
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop
 
display :: DisplayCallback
display = do 
  clear [ColorBuffer]
  renderPrimitive Points $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  flush{ -}
import Graphics.UI.GLUT
         
import Universe

myPoints :: [(GLfloat,GLfloat, GLfloat)]
myPoints = [  (0, 1/2, 0), (0, 1/2, 3)]--(sin (2*pi*k/12), cos (2*pi*k/12)) | k <- [1..12] ]
 
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop
 
display :: DisplayCallback
display = do 
  clear [ColorBuffer]
  renderPrimitive Points $
     mapM_ (\(x, y) -> vertex $ Vertex2 (x*0.9) (y*0.9)) $ map (viewPoint camera) points
--    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  flush