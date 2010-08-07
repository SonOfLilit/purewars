module Main where
import Prelude hiding (lines)
import Control.Monad
import Data.IORef
import Data.Time.Clock
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT

import Game
import Keyboard
import Matrix


fps :: (Fractional a) => a
fps = 1/25

initialWindowSize :: GL.Size
initialWindowSize = GL.Size 640 480

drawOneLine :: GL.Vertex2 Scalar -> GL.Vertex2 Scalar -> IO ()
drawOneLine p1 p2 = GL.renderPrimitive GL.Lines $ do GL.vertex p1; GL.vertex p2

drawLines :: [Line] -> IO ()
drawLines lines = do
  GL.color (GL.Color3 1.0 1.0 1.0 :: GL.Color3 GL.GLfloat)
  forM_ lines (\ (p0, p1) -> drawOneLine (v p0) (v p1))
    where v = uncurry GL.Vertex2

initGL :: IO ()
initGL = do
  GL.clearColor $= GL.Color4 0 0 0 0
  GL.shadeModel $= GL.Flat
  GL.depthFunc $= Nothing

reshape :: GLUT.ReshapeCallback
reshape size@(GL.Size w h) = do
  GL.viewport $= (GL.Position 0 0, size)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho2D 0 (fromIntegral w) 0 (fromIntegral h)

frame :: UTCTime -> 
         IORef Keyboard -> IORef GameState -> 
         LogicStep -> 
         GLUT.TimerCallback
frame lastFrameTime keyboardRef stateRef logicStep = do
  now <- getCurrentTime
  let timeDiff = now `diffUTCTime` lastFrameTime
  
  state <- readIORef stateRef
  keyboard <- readIORef keyboardRef
  let state' = logicStep timeDiff keyboard state
  writeIORef stateRef state'
  GLUT.postRedisplay Nothing
  
  let nextFrameTime = fps `addUTCTime` lastFrameTime
      waitTime = nextFrameTime `diffUTCTime` now
      msWait = truncate (waitTime * 1000)
  GLUT.addTimerCallback msWait (frame now keyboardRef stateRef logicStep)

displayCallback :: IORef GameState -> GLUT.DisplayCallback
displayCallback stateRef = do
  state <- readIORef stateRef
  
  GL.clear [GL.ColorBuffer]
  drawLines $ getLines state
  GLUT.swapBuffers

main :: IO ()
main = do
  _ <- GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode $= [GLUT.DoubleBuffered, GLUT.RGBMode]
  GLUT.initialWindowSize $= initialWindowSize
  _ <- GLUT.createWindow "purewars"
  initGL
  GLUT.reshapeCallback $= Just reshape
  now <- getCurrentTime
  stateRef <- newIORef initialGameState
  keyboardRef <- newIORef initialKeyboardState
  GLUT.keyboardMouseCallback $= Just (keyboardCallback keyboardRef)
  GLUT.displayCallback $= displayCallback stateRef
  GLUT.addTimerCallback 1 (frame now keyboardRef stateRef logic)
  GLUT.mainLoop
