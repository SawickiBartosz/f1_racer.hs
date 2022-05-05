{-#OPTIONS -Wall #-}

module Physics where
import Linear
import FRP.Yampa as Yampa


data Car = Car {
    car_p :: (V2 Float)
   ,v :: (V2 Float)
} deriving Show

data Obstacle = Obstacle {
    obstackle_p :: (V2 Float)
} deriving Show

data Direction = Up | Down | Right | Left

updatePosition :: Car -> Car
updatePosition (Car p v) = Car (p + v) v

checkCollision :: Car -> Obstacle -> Bool
checkCollision (Car p1 _) (Obstacle p2) = distance p1 p2 < 1

hit :: Car -> Car
hit (Car p _) = Car p (return 0 :: V2 Float)

updateVelocity :: Car -> Direction -> Car
updateVelocity (Car p _) Up = Car p (V2 (0::Float) (1::Float))
updateVelocity (Car p _) Down = Car p (V2 (0::Float) (-1::Float))
updateVelocity (Car p _) Physics.Right = Car p (V2 (1::Float) (0::Float))
updateVelocity (Car p _) Physics.Left = Car p (V2 (-1::Float) (0::Float))

spawnCar :: Car
spawnCar = Car (return 0 :: V2 Float) (return 0 :: V2 Float)