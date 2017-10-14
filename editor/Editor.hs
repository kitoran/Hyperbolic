-- import Graphics.UI.Gtk
-- import Graphics.UI.Gtk.OpenGL
 
-- main = do

--    initGUI
--    initGL
--    window <- windowNew

--    conf <- glConfigNew [GLModeRGB, GLModeDepth, GLModeDouble]
--    area <- glDrawingAreaNew conf
--    containerAdd window (toWidget area)

--    widgetShowAll window
--    mainGUI
module Main (main) where

import Data.IORef

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import qualified Graphics.UI.Gtk.Gdk.Events as Gtk
import Graphics.Rendering.OpenGL as GL

import Graphics
import Control.Monad
import Hyperbolic as H
import Physics as P
import qualified Linear as L

level  :: Environment (Double, Double, Double) Double
level = Env (Mesh [(red, P.Polygon [p0, p1, p2])]) ([P.Triangle p0 p1 p2 0.01])
  where p0 = H.Point 1.0 0.0 0.0 2.0
        p1 = rotateAroundZ (tau/3) !$ p0
        p2 = rotateAroundZ (-tau/3) !$ p0
        red = (1.0, 0.0, 0.0)


main :: IO ()
main = do
  Gtk.initGUI

  -- Initialise the Gtk+ OpenGL extension
  -- (including reading various command line parameters)
  GtkGL.initGL

  -- We need a OpenGL frame buffer configuration to be able to create other
  -- OpenGL objects.
  glconfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA,
                                 GtkGL.GLModeDepth,
                                 GtkGL.GLModeDouble]

  -- Create an OpenGL drawing area widget
  canvas <- GtkGL.glDrawingAreaNew glconfig
  Gtk.widgetSetSizeRequest canvas 300 300

  -- Initialise some GL setting just before the canvas first gets shown
  -- (We can't initialise these things earlier since the GL resources that
  -- we are using wouldn't heve been setup yet)
  Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> initialiseGraphics 
  -- do
  --   clearColor $= (Color4 0.0 0.0 0.0 0.0)
  --   matrixMode $= Projection
  --   loadIdentity
  --   ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
  --   depthFunc $= Just Less
  --   drawBuffer $= BackBuffers

  ref <- newIORef (0, 0, 0)
  pos <- newIORef L.identity
  last <- newIORef Nothing
  realized <- newIORef False
  -- Gtk.onRsize canvas
  -- Set the repaint handler
  Gtk.onExpose canvas $ \_ -> do
    GtkGL.withGLDrawingArea canvas $ \glwindow -> do
      p <- readIORef pos
      displayGame (mesh level) $ p L.!*! moveAlongZ (-0.1)--rotateAroundY (-0) L.!*! moveAlongZ (-0.1) L.!*! L.identity
      flush
      GtkGL.glDrawableSwapBuffers glwindow
      writeIORef realized True
    return True
  Gtk.onButtonPress canvas $ \_ -> do
    print "Hey it s me"
    return True
  Gtk.onMotionNotify canvas True (\(Gtk.Motion _ _ x y _ _ _ _) -> do
      e <- readIORef last
      case e of
          Nothing -> return ()
          Just (lx, ly) -> do
              print (lx-x, ly-y)
              modifyIORef pos (rotateAroundZ ((x-lx)/20) L.!*! rotateAroundY ((ly-y)/20) L.!*!) 
      writeIORef last (Just (x,y))
      Gtk.widgetQueueDraw canvas
      return False)
  Gtk.onSizeAllocate canvas $ \(Gtk.Rectangle l u r d) -> do
      print "allocate or die"
      rd <- readIORef realized
      when rd $
        GtkGL.withGLDrawingArea canvas $ \_ -> resize (r-l) (r-d)

  window <- Gtk.windowNew
  Gtk.onDestroy window Gtk.mainQuit
  Gtk.set window [ Gtk.containerBorderWidth := 8,
                   Gtk.windowTitle := "Level editor" ]

  vbox <- Gtk.vBoxNew False 4
  Gtk.set window [ Gtk.containerChild := vbox ]

  Gtk.set vbox [ Gtk.containerChild := canvas
                ]

  Gtk.windowMaximize window
  Gtk.widgetShowAll window
  Gtk.mainGUI

