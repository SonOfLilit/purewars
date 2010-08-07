module Game (Line
            ,getLines
            ,GameState
            ,initialGameState
            ,LogicStep
            ,logic) where
import Data.Time.Clock
import Control.Monad.State

import Matrix


type Line = (Vector2, Vector2)

data GameState = GameState {ship1 :: Ship}
type LogicStep = NominalDiffTime -> State GameState ()

class GameObject a where
  tick :: a -> NominalDiffTime -> IO a
  draw :: a -> [Line]

data Ship = Ship Vector2
instance GameObject Ship where
  tick (Ship (x, y)) t = return $ Ship (x+t', y+2*t') where t' = realToFrac t
  draw (Ship pos) = mapLines (scale 10 #:*#: translate pos #:*:) shipShape

mapLines :: (Vector2 -> Vector2) -> [Line] -> [Line]
mapLines f = map f' 
  where f' (v, u) = (f v, f u)

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

initialGameState :: GameState
initialGameState = GameState {ship1 = Ship (0, 0)}
