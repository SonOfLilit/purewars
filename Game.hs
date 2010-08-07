{-# LANGUAGE NamedFieldPuns #-}
module Game (Line
            ,getLines
            ,GameState
            ,initialGameState
            ,LogicStep
            ,logic) where
import Data.Time.Clock
import qualified Graphics.UI.GLUT as GLUT

import Keyboard
import Matrix


sunMass :: Scalar
sunMass = 1000

type Line = (Vector2, Vector2)

data GameState = GameState {ship1 :: GameObject
                           ,sun :: GameObject}
mapGameState :: (GameObject -> GameObject) -> GameState -> GameState
mapGameState f s@(GameState{ship1, sun}) = s {ship1 = f ship1, sun = f sun}

objects :: GameState -> [GameObject]
objects (GameState{ship1, sun}) = [ship1, sun]

data GameObject = Ship {position, velocity :: Vector2}
                | Sun {position :: Vector2}

type LogicStep = NominalDiffTime -> Keyboard -> GameState -> GameState

tick :: NominalDiffTime -> Keyboard -> GameState -> GameObject -> GameObject
tick t keyboard state ship@(Ship _ _) =
  let t' = realToFrac t
      sumForces = thrust +: gravity
      thrust = if pressed (GLUT.SpecialKey GLUT.KeyUp) keyboard then (5, 0) else zeroV
      gravity = sunForce (position ship) (position $ sun state)
      velocity' = velocity ship +: (t' .*: sumForces)
      position' = position ship +: (t' .*: velocity')
  in ship {position = position', velocity = velocity'}
tick _ _ _ sun@(Sun _) = sun

shape :: GameObject -> [Line]
shape (Ship _ _) = mapLines (scale 10 #:*:) [((-1, -1), (1, 0))
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
logic t keyboard state = mapGameState (tick t keyboard state) state

getLines :: GameState -> [Line]
getLines = concatMap draw . objects

initialGameState :: GameState
initialGameState = GameState {ship1 = Ship {position = zeroV, velocity = zeroV}
                             ,sun = Sun {position = (50, 80)}}

sunForce :: Vector2 -> Vector2 -> Vector2
sunForce shipPos sunPos = (sunMass/(r*r)) .*: a
  where (a, r) = toNormal (sunPos -: shipPos)