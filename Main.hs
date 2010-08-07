module Main where
import Prelude hiding (lines)
import Control.Monad
import Control.Monad.State
import Data.IORef
import Data.Time.Clock
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT

type Length = Double
type Vector2 = (Length, Length)
(.*:) :: Length -> Vector2 -> Vector2
s .*: (x, y) = (s*x, s*y)
(+:) :: Vector2 -> Vector2 -> Vector2
(a, b) +: (a', b') = (a+a', b+b')

type Matrix2 = (Vector2, Vector2)
(.*#) :: Length -> Matrix2 -> Matrix2
s .*# (u, v) = (s .*: u, s .*: v)
(#*:) :: Matrix2 -> Vector2 -> Vector2
(v, u) #*: (x, y) = (x .*: v) +: (y .*: u)
(#*#) :: Matrix2 -> Matrix2 -> Matrix2
m #*# (c, d) = (m #*: c, m #*: d)
type Transformation = (Matrix2, Vector2)
(#:*#:) :: Transformation -> Transformation -> Transformation
(m, v) #:*#: (m', v') = (m #*# m', v +: v')
(#:*:) :: Transformation -> Vector2 -> Vector2
(m, u) #:*: v = (m #*: v) +: u
zeroV = (0, 0)
idM = ((1, 0), (0, 1))

scale k = (k .*# idM, zeroV)
translate v = (idM, v)

type Line = (Vector2, Vector2)
data GameState = GameState {ship1 :: Ship}
type LogicStep = NominalDiffTime -> State GameState ()

class GameObject a where
  tick :: a -> NominalDiffTime -> IO a
  draw :: a -> [Line]

data Ship = Ship Vector2
instance GameObject Ship where
  tick (Ship (x, y)) t = return $ Ship (x+t', y+2*t') where t' = realToFrac t
  draw (Ship pos) = map t2 shipShape
    where t = (scale 10 #:*#: translate pos #:*:)
          t2 (a, b) = (t a, t b)

shipShape :: [Line]
shipShape = [((-1, -1), (1, 0))
            ,((1, 0), (-1, 1))
            ,((-1, 1), (-1, -1))]

logic :: LogicStep
logic t = do
  state <- get
  let d = 0.5 * realToFrac t
      Ship pos = ship1 state
  put state {ship1 = Ship (pos +: (d, 2*d))}

getLines :: GameState -> [Line]
getLines GameState {ship1 = ship} = draw ship

drawOneLine :: GL.Vertex2 Length -> GL.Vertex2 Length -> IO ()
drawOneLine p1 p2 = GL.renderPrimitive GL.Lines $ do GL.vertex p1; GL.vertex p2

drawLines :: [Line] -> IO ()
drawLines lines = do
  GL.color (GL.Color3 1.0 1.0 1.0 :: GL.Color3 GL.GLfloat)
  forM_ lines (\ (p0, p1) -> drawOneLine (v p0) (v p1))
    where v pos = uncurry GL.Vertex2 $ pos

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

fps = 1/25

frame :: UTCTime -> IORef GameState -> LogicStep -> GLUT.TimerCallback
frame lastFrameTime stateRef logicStep = do
  state <- readIORef stateRef
  let ((), state') = runState (logicStep 1.0) state
  writeIORef stateRef state'
  GLUT.postRedisplay Nothing
  
  now <- getCurrentTime
  let nextFrameTime = fps `addUTCTime` lastFrameTime
      waitTime = nextFrameTime `diffUTCTime` now
      msWait = truncate (waitTime * 1000)
  GLUT.addTimerCallback msWait (frame now stateRef logicStep)

display :: IORef GameState -> GLUT.DisplayCallback
display stateRef = do
  state <- readIORef stateRef
  
  GL.clear [GL.ColorBuffer]
  drawLines $ getLines state
  GLUT.swapBuffers

main :: IO ()
main = do
  _ <- GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode $= [GLUT.DoubleBuffered, GLUT.RGBMode]
  GLUT.initialWindowSize $= GL.Size 640 480
  _ <- GLUT.createWindow "purewars"
  initGL
  GLUT.reshapeCallback $= Just reshape
  now <- getCurrentTime
  stateRef <- newIORef GameState {ship1 = Ship (0, 0)}
  GLUT.displayCallback $= display stateRef
  GLUT.addTimerCallback 1 (frame now stateRef logic)
  GLUT.mainLoop
