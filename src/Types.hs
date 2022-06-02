module Types where
import Data.Vector2
import FRP.Yampa
import Graphics.Gloss.Data.Color
import qualified Graphics.Gloss.Interface.IO.Game as G

type InputEvent = G.Event

data World = World {
    cars :: [Car] 
   ,time :: Time
} deriving Show

data Car = Car {
    car_p :: Pos
   ,car_v :: Vel
   ,col :: Color
} deriving Show

type Force = Vector2 Float
type Acc = Vector2 Float
type Vel = Vector2 Float
type Pos = Vector2 Float
type Mass = Float

data Direction = Up | Down | Right | Left | None

data Obstacle = Obstacle {
    obstackle_p :: Vector2 Float
} deriving Show

