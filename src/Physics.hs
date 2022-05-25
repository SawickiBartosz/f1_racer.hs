{-#OPTIONS -Wall #-}

module Physics where
import FRP.Yampa as Yampa
import Data.Vector2

data Car = Car {
    car_p :: Vector2 Float
   ,v :: Vector2 Float
} deriving Show

data Obstacle = Obstacle {
    obstackle_p :: Vector2 Float
} deriving Show

data Direction = Up | Down | Right | Left | None

updatePosition :: Car -> Car
updatePosition (Car p curr_v) = Car (p ^+^ curr_v) curr_v

checkCollision :: Car -> Obstacle -> Bool
checkCollision (Car p1 _) (Obstacle p2) = norm (p1 ^-^ p2) < 1

hit :: Car -> Car
hit (Car p _) = Car p (zeroVector)

updateVelocity :: Car -> Direction -> Car
updateVelocity (Car p _) Up = Car p (vector2 (0::Float) (1::Float))
updateVelocity (Car p _) Down = Car p (vector2 (0::Float) (-1::Float))
updateVelocity (Car p _) Physics.Right = Car p (vector2 (1::Float) (0::Float))
updateVelocity (Car p _) Physics.Left = Car p (vector2 (-1::Float) (0::Float))

updateCar :: Car -> Direction -> Car
updateCar car dir = updatePosition $ updateVelocity car dir

spawnCar :: Car
spawnCar = Car (zeroVector) (vector2 0.5 0)