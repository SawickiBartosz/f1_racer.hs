{-# LANGUAGE DeriveGeneric #-}

module Types where
import Data.Vector2
import FRP.Yampa
import Graphics.Gloss.Data.Color
import GHC.Generics (Generic)
import Control.Monad (mzero)
import Data.Csv
import Data.List
import Data.Vector
import Data.List.Split
import qualified Graphics.Gloss.Interface.IO.Game as G

type InputEvent = G.Event

data World = World {
    cars :: [Car]
   ,obstacles :: [Obstacle]
   ,time :: Time
} deriving Show

data Car = Car {
    car_p :: Pos
   ,car_v :: Vel
   ,col :: Color
} deriving (Show, Generic)

type Force = Vector2 Float
type Acc = Vector2 Float
type Vel = Vector2 Float
type Pos = Vector2 Float
type Mass = Float
type Size = Vector2 Float

data Direction = Up | Down | Right | Left | None

data Obstacle = Obstacle {
    pos :: !Pos
   ,size :: !Size
   ,slowingForce :: !Float
} deriving (Generic, Show)

data ParsableObstacle  = ParsableObstacle{
    posx :: Float
   ,posy :: Float
   ,sizex :: Float
   ,sizey :: Float
   ,sf :: Float
} deriving (Generic, Show)

fromParsableObstacle :: ParsableObstacle -> Obstacle
fromParsableObstacle (ParsableObstacle px py sx sy sf) = Obstacle (vector2 px py) (vector2 sx sy) sf

toParsableObstacle :: Obstacle -> ParsableObstacle
toParsableObstacle (Obstacle p s sf) = ParsableObstacle (vector2X p) (vector2Y p) (vector2X s) (vector2Y s) sf


instance ToRecord ParsableObstacle
instance FromRecord ParsableObstacle
