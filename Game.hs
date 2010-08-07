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


ship1ThrustKey, ship1CCWKey, ship1CWKey :: GLUT.Key
ship1ThrustKey = GLUT.SpecialKey GLUT.KeyUp
ship1CCWKey = GLUT.SpecialKey GLUT.KeyRight
ship1CWKey = GLUT.SpecialKey GLUT.KeyLeft

thrustForce :: Scalar
thrustForce = 20
engineTorque :: Scalar
engineTorque = 3

sunMass :: Scalar
sunMass = 100000

type Line = (Vector2, Vector2)

data GameState = GameState {ship1 :: GameObject
                           ,sun :: GameObject}
mapGameState :: (GameObject -> GameObject) -> GameState -> GameState
mapGameState f s@(GameState{ship1, sun}) = s {ship1 = f ship1, sun = f sun}

objects :: GameState -> [GameObject]
objects (GameState{ship1, sun}) = [ship1, sun]

data GameObject = Ship {position, velocity :: Vector2
                       ,angle, angleVelocity :: Scalar}
                | Sun {position :: Vector2, angle :: Scalar}

type LogicStep = NominalDiffTime -> Keyboard -> GameState -> GameState

tick :: NominalDiffTime -> Keyboard -> GameState -> GameObject -> GameObject
tick t keyboard state ship@(Ship _ _ _ _) =
  let t' = realToFrac t
      sumForces = thrust +: gravity
      thrust = if pressed ship1ThrustKey keyboard 
               then rotate (angle ship) #:*: (thrustForce, 0)
               else zeroV
      gravity = sunForce (position ship) (position $ sun state)
      velocity' = velocity ship +: (t' .*: sumForces)
      position' = position ship +: (t' .*: velocity')
      torque = case (pressed ship1CCWKey keyboard, pressed ship1CWKey keyboard) of
        (True, False) -> engineTorque
        (False, True) -> -engineTorque
        _ -> 0
      angleVelocity' = angleVelocity ship + t' * torque
      angle' = angle ship + t' * angleVelocity'
  in ship {position = position'
          ,velocity = velocity'
          ,angle = angle'
          ,angleVelocity = angleVelocity'}
tick _ _ _ sun@(Sun _ _) = sun

shape :: GameObject -> [Line]
shape (Ship _ _ _ _) = mapLines (scale 10 #:*:) [((-1, -1), (1, 0))
                                                ,((1, 0), (-1, 1))
                                                ,((-1, 1), (-1, -1))]
shape (Sun _ _) = [((-1, -1), (1, 0))
                ,((1, 0), (-1, 1))
                ,((-1, 1), (-1, -1))]


draw :: GameObject -> [Line]
draw o = mapLines (translate (position o) #:*#: rotate (angle o) #:*:) $ shape o

mapLines :: (Vector2 -> Vector2) -> [Line] -> [Line]
mapLines f = map f' 
  where f' (v, u) = (f v, f u)

logic :: LogicStep
logic t keyboard state = mapGameState (tick t keyboard state) state

getLines :: GameState -> [Line]
getLines = concatMap draw . objects

initialGameState :: GameState
initialGameState = GameState {ship1 = Ship {position = zeroV
                                           ,velocity = zeroV
                                           ,angle = 0
                                           ,angleVelocity = 0}
                             ,sun = Sun {position = (200, 300)
                                        ,angle = 0}}

sunForce :: Vector2 -> Vector2 -> Vector2
sunForce shipPos sunPos = (sunMass/(r*r)) .*: a
  where (a, r) = toNormal (sunPos -: shipPos)