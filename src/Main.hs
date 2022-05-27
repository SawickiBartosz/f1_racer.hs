-- {-# LANGUAGE Arrows #-}
-- module Main where

-- import Control.Monad
import FRP.Yampa
import Physics
import PlayYampa
import Types
import Control.Concurrent
import Data.Vector2
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G
-- import Graphics.Gloss
-- import qualified Graphics.Gloss.Interface.IO.Game as G


-- type Pos = Vector2 Float

-- type InputEvent = G.Event

main :: IO ()
main = do
  playYampa
      (InWindow "2048 game" (410, 500) (200, 200))
      white
      30
      mainSF

mainSF :: SF (Event InputEvent) Picture
mainSF = constant () >>> time >>> Physics.simulate >>> render

render :: SF (Pos, Vel) Picture
render = arrPrim (\(p,v) -> translate (vector2X p) (vector2Y p) (circle 5))

-- mainLoop :: GameState -> SF (Event InputEvent) GameState
-- mainLoop (GameState cars score status) = parseInput >>> (runGame  startGame)

-- parseInput :: SF (Event InputEvent) GameInput
-- parseInput = arr $ \event ->
--   case event of
--     Event (G.EventKey (G.SpecialKey G.KeyUp) G.Down _ _) -> event `tag` Physics.Up
--     Event (G.EventKey (G.SpecialKey G.KeyDown) G.Down _ _) -> event `tag` Physics.Down
--     Event (G.EventKey (G.SpecialKey G.KeyLeft) G.Down _ _) -> event `tag` Physics.Left
--     Event (G.EventKey (G.SpecialKey G.KeyRight) G.Down _ _) -> event `tag` Physics.Right
--     _ -> event `tag` Physics.None

-- runGame :: GameState -> SF GameInput GameState
-- runGame state = proc input -> do
--   rec currentState <- dHold state -< gameUpdated
--       gameUpdated <- arr update -< (currentState, input)

--   returnA -< currentState

-- update :: (GameState, GameInput) -> Event GameState
-- update ((GameState cars score status), input) =
--     case input of
--       Event None -> Event $ GameState (map (\c -> updateCar c Physics.Up) cars) (score+1) status
--       Event direction -> Event $ GameState (map (\c -> updateCar c direction) cars) (score+1) status
--       _ -> Event $ GameState (map (\c -> updateCar c Physics.Down) cars) (score+1) status

-- startGame :: GameState
-- startGame = GameState ([Car zeroVector zeroVector]) 0 InProgress 

-- data GameStatus = InProgress
--                 | GameOver
--                 deriving (Eq, Show)

-- type Cars = [Car]

-- data GameState = GameState { cars :: Cars
--                , score :: Int
--                , status :: GameStatus
--                } deriving (Show)

-- type GameInput = Event Physics.Direction

