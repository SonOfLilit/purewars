{-# LANGUAGE NamedFieldPuns #-}
module Game (Line
            ,getLines
            ,GameState
            ,initialGameState
            ,LogicStep
            ,logic) where
import Data.Fixed(mod')
import Data.Time.Clock
import qualified Graphics.UI.GLUT as GLUT

import Keyboard
import Matrix


ship1ThrustKey, ship1CCWKey, ship1CWKey, ship1ShootKey :: GLUT.Key
ship1ThrustKey = GLUT.SpecialKey GLUT.KeyUp
ship1CCWKey = GLUT.SpecialKey GLUT.KeyRight
ship1CWKey = GLUT.SpecialKey GLUT.KeyLeft
ship1ShootKey = GLUT.Char '/'

thrustForce :: Scalar
thrustForce = 60
engineTorque :: Scalar
engineTorque = 5
engineTorqueDampening :: Scalar
engineTorqueDampening = 2
cannonLoadingTime :: NominalDiffTime
cannonLoadingTime = 0.5
shotLimit :: Int
shotLimit = 5
shotLifetime :: NominalDiffTime
shotLifetime = 5
shotDistance :: Scalar
shotDistance = 11
shotVelocity :: Scalar
shotVelocity = 25

sunMass :: Scalar
sunMass = 2000000

-- TODO: Remove this hack
screenWidth, screenHeight :: Scalar
screenWidth = 640
screenHeight = 480

type Line = (Vector2, Vector2)

data GameState = GameState {ship1 :: GameObject
                           ,shots :: [GameObject]
                           ,sun :: GameObject}
mapGameState :: (GameObject -> GameObject) -> GameState -> GameState
mapGameState f s@(GameState{ship1, shots, sun}) = s {ship1 = f ship1
                                                    ,shots = map f shots
                                                    ,sun = f sun}

objects :: GameState -> [GameObject]
objects (GameState{ship1, shots, sun}) = [ship1, sun] ++ shots

data GameObject = Ship {position, velocity :: Vector2
                       ,angle, angleVelocity :: Scalar
                       ,shotTimer :: NominalDiffTime}
                | Shot {position, velocity :: Vector2
                       ,angle :: Scalar
                       ,shotTimer :: NominalDiffTime}
                | Sun {position :: Vector2, angle :: Scalar}

type LogicStep = NominalDiffTime -> Keyboard -> GameState -> GameState

tick :: NominalDiffTime -> Keyboard -> GameState -> GameObject -> GameObject
tick t keyboard state ship@(Ship _ _ _ _ _) =
  let thrust = if pressed ship1ThrustKey keyboard 
               then rotate (angle ship) #:*: (thrustForce, 0)
               else zeroV
      p key = pressed key keyboard
      torque = case (p ship1CCWKey, p ship1CWKey) of
        (True, False) -> engineTorque
        (False, True) -> -engineTorque
        _ -> -engineTorqueDampening * angleVelocity ship
  in applyPhysics thrust torque t state . updateShotTimer t $ ship
tick t _ state shot@(Shot _ _ _ _) = 
  applyPhysics zeroV 0 t state . updateShotTimer t $ shot
tick _ _ _ o = o

updateShotTimer :: NominalDiffTime -> GameObject -> GameObject
updateShotTimer t o = o {shotTimer = max 0 $ shotTimer o - realToFrac t}

-- notice how angles are not calculated when not a ship. Lazy
-- evaluation is pretty
applyPhysics :: Vector2 -> Scalar -> 
                NominalDiffTime -> GameState -> 
                GameObject -> GameObject
applyPhysics force torque t state o =
  let t' = realToFrac t
      sumForces = force +: gravity
      gravity = sunForce (position o) (position $ sun state)
      velocity' = velocity o +: (t' .*: sumForces)
      position' = let (x, y) = position o +: (t' .*: velocity')
                  in (x `mod'` screenWidth, y `mod'` screenHeight)
      angleVelocity' = angleVelocity o + t' * torque
      angle' = angle o + t' * angleVelocity'
  in case o of
    (Ship _ _ _ _ _) ->
      o {position = position'
        ,velocity = velocity'
        ,angle = angle'
        ,angleVelocity = angleVelocity'}
    _ ->
      o {position = position'
        ,velocity = velocity'}

sunForce :: Vector2 -> Vector2 -> Vector2
sunForce shipPos sunPos = (sunMass/(r*r)) .*: a
  where (a, r) = toNormal (sunPos -: shipPos)

shape :: GameObject -> [Line]
shape (Ship _ _ _ _ _) = mapLines (scale 10 #:*:) [((-1, -1), (1, 0))
                                                  ,((1, 0), (-1, 1))
                                                  ,((-1, 1), (-1, -1))]
shape _ = [((-1, -1), (1, 0))
                ,((1, 0), (-1, 1))
                ,((-1, 1), (-1, -1))]

draw :: GameObject -> [Line]
draw o = mapLines (translate (position o) #:*#: rotate (angle o) #:*:) $ shape o

mapLines :: (Vector2 -> Vector2) -> [Line] -> [Line]
mapLines f = map f' 
  where f' (v, u) = (f v, f u)

logic :: LogicStep
logic t keyboard state = 
  let state' = mapGameState (tick t keyboard state) state
      ship1' = ship1 state'
      shoot = pressed ship1ShootKey keyboard 
           && shotTimer ship1' == 0 
           && length (shots state') < shotLimit
      ship1'' = if shoot 
               then ship1' {shotTimer = cannonLoadingTime}
               else ship1'
      shots' = filter ((>0) . shotTimer) (shots state')
      shots'' = if shoot
              then (newShot $ ship1 state') : shots'
              else shots'
  in state' {ship1 = ship1'', shots = shots''}

-- TODO: Better handling of shooting backwards of ship velocity
newShot :: GameObject -> GameObject
newShot ship = Shot {position = position ship +: (shotDistance .*: direction)
                    ,velocity = velocity ship +: (shotVelocity .*: direction)
                    ,angle = 0
                    ,shotTimer = shotLifetime}
  where direction = rotate (angle ship) #:*: (1, 0)

getLines :: GameState -> [Line]
getLines = concat . (++limitRect) . map draw . objects
  where limitRect = [[((1, 1), (1, h)), ((1, h), (w, h)),
                      ((w, h), (w, 1)), ((w, 1), (1, 1))]]
        w = screenWidth
        h = screenHeight

initialGameState :: GameState
initialGameState = GameState {ship1 = Ship {position = zeroV
                                           ,velocity = zeroV
                                           ,angle = 0
                                           ,angleVelocity = 0
                                           ,shotTimer = 0}
                             ,shots = []
                             ,sun = Sun {position = (screenWidth/2
                                                    ,screenHeight/2)
                                        ,angle = 0}}
