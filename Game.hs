{-# LANGUAGE NamedFieldPuns #-}

module Game (Line
            ,getLines
            ,GameState
            ,initialGameState
            ,LogicStep
            ,logic) where
import Data.Time.Clock

import Matrix


type Line = (Vector2, Vector2)

data GameState = GameState {ship1 :: GameObject
                           ,sun :: GameObject}
mapGameStateM :: Monad m => (GameObject -> m GameObject) -> GameState -> m GameState
mapGameStateM f s@(GameState{ship1, sun}) = do
  ship1' <- f ship1
  sun' <- f sun
  return s {ship1 = ship1', sun = sun'}

objects :: GameState -> [GameObject]
objects (GameState{ship1, sun}) = [ship1, sun]

data GameObject = Ship {position :: Vector2}
                | Sun {position :: Vector2}

type LogicStep = NominalDiffTime -> GameState -> IO GameState

tick :: NominalDiffTime -> GameObject -> IO GameObject
tick t ship@(Ship _) = return $ ship {position = position ship +: (t', 2*t')} where t' = realToFrac t
tick _ sun@(Sun _) = return sun

shape :: GameObject -> [Line]
shape (Ship _) = mapLines (scale 10 #:*:) [((-1, -1), (1, 0))
                                          ,((1, 0), (-1, 1))
                                          ,((-1, 1), (-1, -1))]
shape (Sun _) = [((-1, -1), (1, 0))
                ,((1, 0), (-1, 1))
                ,((-1, 1), (-1, -1))]


draw :: GameObject -> [Line]
draw o = mapLines (translate (position o) #:*:) $ shape o

mapLines :: (Vector2 -> Vector2) -> [Line] -> [Line]
mapLines f = map f' 
  where f' (v, u) = (f v, f u)

logic :: LogicStep
logic t state = mapGameStateM (tick t) state

getLines :: GameState -> [Line]
getLines = concat . map draw . objects

initialGameState :: GameState
initialGameState = GameState {ship1 = Ship {position = (0, 0)}
                             ,sun = Sun {position = (100, 200)}}
