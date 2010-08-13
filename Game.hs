{-# LANGUAGE NamedFieldPuns #-}
module Game (Line
            ,getLines
            ,GameState
            ,initialGameState
            ,LogicStep
            ,logic) where
import Control.Monad.State
import Data.Fixed(mod')
import Data.Time.Clock
import qualified Graphics.UI.GLUT as GLUT

import Keyboard
import Matrix

data ShipControls = Controls{thrustKey, ccwKey, cwKey, shootKey :: GLUT.Key}
controls1, controls2 :: ShipControls
controls1 = Controls{thrustKey = GLUT.SpecialKey GLUT.KeyUp
                    ,ccwKey = GLUT.SpecialKey GLUT.KeyRight
                    ,cwKey = GLUT.SpecialKey GLUT.KeyLeft
                    ,shootKey = GLUT.Char '/'}
controls2 = Controls{thrustKey = GLUT.Char 'w'
                    ,ccwKey = GLUT.Char 'd'
                    ,cwKey = GLUT.Char 'a'
                    ,shootKey = GLUT.Char 'f'}

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
shotDistance = 17
shotVelocity :: Scalar
shotVelocity = 25

sunMass :: Scalar
sunMass = 2000000

-- TODO: Remove this hack
screenWidth, screenHeight :: Scalar
screenWidth = 640
screenHeight = 480

type Transform a = a -> a

type Line = (Vector2, Vector2)

data GameStatus = Menu | Playing | Win

data GameState = GameState{status :: GameStatus
                          ,ship1, ship2 :: GameObject
                          ,shots1, shots2 :: [GameObject]
                          ,sun :: GameObject}
mapGameState :: Transform GameObject -> Transform GameState
mapGameState f s@(GameState{ship1, ship2, shots1, shots2, sun}) = 
  s{ship1 = f ship1
   ,ship2 = f ship2
   ,shots1 = map f shots1
   ,shots2 = map f shots2
   ,sun = f sun}

objects :: GameState -> [GameObject]
objects (GameState{ship1, ship2, shots1, shots2, sun}) = [ship1, ship2, sun] ++ shots1 ++ shots2

allShots :: GameState -> [GameObject]
allShots state = shots1 state ++ shots2 state

data GameObject = Ship{position, velocity :: Vector2
                      ,angle, angleVelocity :: Scalar
                      ,shotTimer :: NominalDiffTime
                      ,controls :: ShipControls
                      ,setShip :: GameObject -> Transform GameState
                      ,getShipShots :: GameState -> [GameObject]
                      ,setShipShots :: [GameObject] -> Transform GameState}
                | Shot{position, velocity :: Vector2
                      ,angle :: Scalar
                      ,shotTimer :: NominalDiffTime}
                | Sun{position :: Vector2, angle :: Scalar}
halfHeight, radius :: GameObject -> Scalar
halfHeight (Ship _ _ _ _ _ _ _ _ _) = 10
halfHeight (Shot _ _ _ _) = 0.5
halfHeight (Sun _ _) = 1
radius o = sqrt 2 * halfHeight o

type LogicStep = NominalDiffTime -> Keyboard -> Transform GameState

tick :: NominalDiffTime -> Keyboard -> GameState -> Transform GameObject
tick t keyboard state ship@(Ship _ _ _ _ _ _ _ _ _) =
  let p f = pressed (f (controls ship)) keyboard
      thrust = if p thrustKey
               then rotate (angle ship) #:*: (thrustForce, 0)
               else zeroV
      torque = case (p ccwKey, p cwKey) of
        (True, False) -> engineTorque
        (False, True) -> -engineTorque
        _ -> -engineTorqueDampening * angleVelocity ship
  in applyPhysics thrust torque t state . updateShotTimer t $ ship
tick t _ state shot@(Shot _ _ _ _) = 
  applyPhysics zeroV 0 t state . updateShotTimer t $ shot
tick _ _ _ o = o

updateShotTimer :: NominalDiffTime -> Transform GameObject
updateShotTimer t o = o{shotTimer = max 0 (shotTimer o - realToFrac t)}

-- notice how angles are not calculated when not a ship. Lazy
-- evaluation is pretty
applyPhysics :: Vector2 -> Scalar -> 
                NominalDiffTime -> GameState -> 
                Transform GameObject
applyPhysics force torque t state o =
  let t' = realToFrac t
      sumForces = force +: gravity
      gravity = sunForce (position o) (position (sun state))
      velocity' = velocity o +: (t' .*: sumForces)
      position' = let (x, y) = position o +: (t' .*: velocity')
                  in (x `mod'` screenWidth, y `mod'` screenHeight)
      angleVelocity' = angleVelocity o + t' * torque
      angle' = angle o + t' * angleVelocity'
  in case o of
    (Ship _ _ _ _ _ _ _ _ _) ->
      o{position = position'
       ,velocity = velocity'
       ,angle = angle'
       ,angleVelocity = angleVelocity'}
    _ ->
      o{position = position'
       ,velocity = velocity'}

sunForce :: Vector2 -> Vector2 -> Vector2
sunForce shipPos sunPos = (sunMass/(r*r)) .*: a
  where (a, r) = toNormal (sunPos -: shipPos)

shape :: GameObject -> [Line]
shape (Ship _ _ _ _ _ _ _ _ _) = [((-1, -1), (1, 0))
                         ,((1, 0), (-1, 1))
                         ,((-1, 1), (-1, -1))]
shape _ = [((-1, -1), (1, 0))
          ,((1, 0), (-1, 1))
          ,((-1, 1), (-1, -1))]

draw :: GameObject -> [Line]
draw o = mapLines (transform  #:*:) (shape o)
  where transform = translate (position o) #:*#: 
                    rotate (angle o) #:*#: 
                    scale (halfHeight o)

mapLines :: (Vector2 -> Vector2) -> [Line] -> [Line]
mapLines f = map f' 
  where f' (v, u) = (f v, f u)

logic :: LogicStep
logic t keyboard state@(GameState{status=Playing}) = execState logic' state
  where
    logic' = do
      let theSun = sun state
          shotFilter shot = shotTimer shot > 0 && not (collision theSun shot)
      modify (\s-> mapGameState (tick t keyboard s) s)
      modify . modifyShots $ filter shotFilter
      modify . modifyShips $ handleShooting keyboard
      modify . modifyShips $ shipCollisions
logic t keyboard s@(GameState{status=Win}) = execState logic' s
  where updateShipShotTimer :: GameObject -> Transform GameState
        updateShipShotTimer ship = setShip ship (updateShotTimer t ship)
        logic' :: State GameState ()
        logic' = do
          modify . modifyShips $ updateShipShotTimer
          modify . modifyShips $ handleOkPress
        handleOkPress ship state =
          let okPressed = pressed (shootKey $ controls ship) keyboard 
                          && shotTimer ship == 0 
          in if okPressed
             then initialGameState
             else state

modifyShots :: Transform [GameObject] -> Transform GameState
modifyShots f s = s{shots1=f (shots1 s), shots2=f (shots2 s)}
modifyShips :: (GameObject -> Transform GameState) -> Transform GameState
modifyShips f s = f (ship1 s) . f (ship2 s) $ s    

shipCollisions :: GameObject -> Transform GameState
shipCollisions ship state =
  let shots = allShots state
      theSun = sun state
      isHit = any (collision ship) shots || collision ship theSun
  in if isHit
     then state{status=Win}
     else state

collision :: GameObject -> GameObject -> Bool
collision o1 o2 = absV centersV <= radiusSum
  where centersV = position o2 -: position o1
        radiusSum = radius o1 + radius o2

handleShooting :: Keyboard -> GameObject -> Transform GameState
handleShooting keyboard ship state =
  let shots = getShipShots ship $ state
      shoot = pressed (shootKey $ controls ship) keyboard 
              && shotTimer ship == 0 
              && length shots < shotLimit
  in if shoot
    then setShipShots ship (newShot ship : shots) . 
         setShip ship ship{shotTimer = cannonLoadingTime} $
         state
    else state

-- TODO: Better handling of shooting backwards of ship velocity
newShot :: GameObject -> GameObject
newShot ship = Shot{position = position ship +: (shotDistance .*: direction)
                   ,velocity = velocity ship +: (shotVelocity .*: direction)
                   ,angle = 0
                   ,shotTimer = shotLifetime}
  where direction = rotate (angle ship) #:*: (1, 0)

getLines :: GameState -> [Line]
getLines state@(GameState{status=Playing}) =
  concat . (++[limitRect]) . map draw . objects $ state
getLines GameState{status=Win} = limitRect

limitRect :: [Line]
limitRect = [((1, 1), (1, h)), ((1, h), (w, h)),
             ((w, h), (w, 1)), ((w, 1), (1, 1))]
  where w = screenWidth
        h = screenHeight


data Player = Player1 | Player2

initialGameState :: GameState
initialGameState = GameState{status = Playing
                            ,ship1 = initialShipState Player1
                            ,ship2 = initialShipState Player2
                            ,shots1 = []
                            ,shots2 = []
                            ,sun = Sun{position = (screenWidth/2
                                                  ,screenHeight/2)
                                      ,angle = 0}}

initialShipState :: Player -> GameObject
initialShipState player = Ship{position = case player of
                                  Player1 -> (20, 20)
                                  Player2 -> (640, 480) -: (20, 20)
                              ,velocity = zeroV
                              ,angle = case player of
                                Player1 -> 0
                                Player2 -> pi
                              ,angleVelocity = 0
                              ,shotTimer = cannonLoadingTime
                              ,controls = case player of
                                Player1 -> controls1
                                Player2 -> controls2
                              ,setShip = case player of
                                Player1 -> \ship s-> s{ship1 = ship}
                                Player2 -> \ship s-> s{ship2 = ship}
                              ,getShipShots = case player of
                                Player1 -> shots1
                                Player2 -> shots2
                              ,setShipShots = case player of
                                Player1 -> \shots s-> s{shots1 = shots}
                                Player2 -> \shots s-> s{shots2 = shots}}