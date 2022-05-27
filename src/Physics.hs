{-#OPTIONS -Wall #-}

module Physics where
import FRP.Yampa as Yampa
import FRP.Yampa.Arrow
import Data.Vector2

data Car = Car {
    car_p :: Vector2 Float
   ,car_v :: Vector2 Float
   ,car_a :: Vector2 Float
   ,car_m :: Float
} deriving Show

type Force = Vector2 Float
type Acc = Vector2 Float
type Vel = Vector2 Float
type Pos = Vector2 Float
type Mass = Float

reduceVel:: SF Vel Vel
reduceVel = loopPre (0.99::Float) (arrPrim (\(v1, c) -> (c *^ v1, c*0.99)))

updatePosVel :: SF Acc (Pos, Vel)
updatePosVel = integral >>> reduceVel >>> (integral &&& identity)

addForce :: Mass -> Force -> SF Time Acc
addForce m force = constant (force ^/ m)

addForce' :: Mass -> Force -> SF Acc Acc
addForce' m force = constant (force ^/ m)

generateForceOnce :: Time ->  SF Time (Acc, Event Force)
generateForceOnce t = (constant zeroVector) &&& (after t (vector2 100.0 10.0))

generateForceOnce' :: Time ->  SF Acc (Acc, Event Force)
generateForceOnce' t = identity &&& (after t (vector2 0 200.0))


stopForce :: Time -> SF Acc (Acc, Event Force)
stopForce t = identity &&& after t zeroVector

simulate :: SF Time (Pos, Vel)
simulate = switch (generateForceOnce 1) (addForce 1.0) >>> 
            switch (stopForce 2) (addForce' 1.0) >>> 
            switch (generateForceOnce' 3) (addForce' 1.0) >>> 
            updatePosVel 

generateForce :: SF () (Event Force)
generateForce = repeatedly 1 (vector2 1.0 1.0)

data Obstacle = Obstacle {
    obstackle_p :: Vector2 Float
} deriving Show

data Direction = Up | Down | Right | Left | None

-- updatePosition :: Car -> Car
-- updatePosition (Car p curr_v) = Car (p ^+^ curr_v) curr_v

-- checkCollision :: Car -> Obstacle -> Bool
-- checkCollision (Car p1 _) (Obstacle p2) = norm (p1 ^-^ p2) < 1

-- hit :: Car -> Car
-- hit (Car p _) = Car p (zeroVector)

-- updateVelocity :: Car -> Direction -> Car
-- updateVelocity (Car p _) Up = Car p (vector2 (0::Float) (1::Float))
-- updateVelocity (Car p _) Down = Car p (vector2 (0::Float) (-1::Float))
-- updateVelocity (Car p _) Physics.Right = Car p (vector2 (1::Float) (0::Float))
-- updateVelocity (Car p _) Physics.Left = Car p (vector2 (-1::Float) (0::Float))

-- updateCar :: Car -> Direction -> Car
-- updateCar car dir = updatePosition $ updateVelocity car dir

spawnCar :: Car
spawnCar = Car (zeroVector) (zeroVector) (zeroVector) 1