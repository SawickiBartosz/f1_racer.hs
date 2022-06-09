{-# LANGUAGE DeriveGeneric #-}
{-#OPTIONS -Wall #-}

module Types where
import Data.Vector2
import FRP.Yampa
import Graphics.Gloss.Data.Color
import GHC.Generics (Generic)
import Data.Csv
import qualified Graphics.Gloss.Interface.IO.Game as G

type InputEvent = G.Event

data GameState = Running | Finished deriving (Show, Eq)

instance Ord GameState where
    compare Running Finished = LT
    compare Finished Running = GT
    compare _ _ = EQ

data World = World {
    cars :: [Car]
   ,obstacles :: [Obstacle]
   ,finish :: Obstacle
   ,times :: [Time]
   ,state :: GameState
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

data Direction = Up | Down | Right | Left | None deriving Eq

data Controls = Space | P1 | P2 | R | W | S | A | D | KeyUp | KeyDown | KeyRight | KeyLeft | NoControl deriving Eq

controlToDirection :: Controls -> Event [Controls] -> Event [Direction]
controlToDirection P1 e = if isEvent e then catEvents $ map tagControl (fromEvent e) else NoEvent where 
    tagControl x = case x of 
        KeyDown -> Event Down
        KeyUp -> Event Up
        KeyLeft -> Event Types.Left
        KeyRight -> Event Types.Right
        _ -> Event None

controlToDirection P2 e = if isEvent e then catEvents $ map tagControl (fromEvent e) else NoEvent where 
    tagControl x = case x of 
        S -> Event Down
        W -> Event Up
        A -> Event Types.Left
        D -> Event Types.Right
        _ -> Event None

data Obstacle = Obstacle {
    pos :: !Pos
   ,size :: !Size
   ,slowingForce :: !Float
} deriving (Generic, Show)

type Finish = Obstacle

data ParsableObstacle  = ParsableObstacle{
    posx :: Float
   ,posy :: Float
   ,sizex :: Float
   ,sizey :: Float
   ,sf :: Float
} deriving (Generic, Show)

fromParsableObstacle :: ParsableObstacle -> Obstacle
fromParsableObstacle (ParsableObstacle px py sx sy sf') = Obstacle (vector2 px py) (vector2 sx sy) sf'

toParsableObstacle :: Obstacle -> ParsableObstacle
toParsableObstacle (Obstacle p s sf') = ParsableObstacle (vector2X p) (vector2Y p) (vector2X s) (vector2Y s) sf'


instance ToRecord ParsableObstacle
instance FromRecord ParsableObstacle
