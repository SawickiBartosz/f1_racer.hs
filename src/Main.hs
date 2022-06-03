-- {-# LANGUAGE Arrows #-}
module Main where

import FRP.Yampa
import Physics
import PlayYampa
import Types
import Control.Concurrent
import Data.Vector2
import Data.Csv
import qualified Data.Vector as DV
import qualified Data.ByteString.Lazy as BL
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G


main :: IO ()
main = do
  csvData <- BL.readFile "world.csv"
  obstacles <- case ((decode NoHeader csvData) :: Either String (DV.Vector ParsableObstacle)) of
        Prelude.Left err -> return []
        Prelude.Right x -> return $ DV.toList $ DV.map fromParsableObstacle x
  playYampa
      (InWindow "F1 Racer" (800, 600) (200, 200))
      white
      60
      (mainSF obstacles)

mainSF :: [Obstacle] ->  SF (Event InputEvent) Picture
mainSF obstacles = ((FRP.Yampa.time &&& parseInput) &&& (constant obstacles)) >>> (Physics.simulate &&& FRP.Yampa.time) >>> arr makeWorld >>> arr renderWorld


parseInput :: SF (Event InputEvent) (Event Direction)
parseInput = arr $ \event ->
  case event of
    Event (G.EventKey (G.SpecialKey G.KeyUp) G.Down _ _) -> event `tag` Types.Up
    Event (G.EventKey (G.SpecialKey G.KeyDown) G.Down _ _) -> event `tag` Types.Down
    Event (G.EventKey (G.SpecialKey G.KeyLeft) G.Down _ _) -> event `tag` Types.Left
    Event (G.EventKey (G.SpecialKey G.KeyRight) G.Down _ _) -> event `tag` Types.Right
    _ -> event `tag` None
    -- // TODO trigger directions if key is crrently pressed 

makeWorld :: ((Pos, Vel, [Obstacle]), Time) -> World
makeWorld ((p,v, obs), t) = World ([Car p v red]) obs t

renderWorld :: World -> Picture
renderWorld (World cars obstacles t) = pictures $ (map renderCar cars) ++ (map renderObstacle obstacles) ++ [scale 0.1 0.1 (text $ show $ round t)]

renderCar :: Car -> Picture
renderCar (Car p v c) =  Color c $ pictures [translate (vector2X p) (vector2Y p) (ThickCircle 5 10), 
                                             translate 50 200 $ scale 0.1 0.1 (text $ show v)]

renderObstacle :: Obstacle -> Picture
renderObstacle (Obstacle p s _) = polygon $ pathFromPosSize p s

pathFromPosSize :: Pos -> Vector2 Float -> Path
pathFromPosSize p s = [(vector2X p, vector2Y p), 
                       (vector2X p + vector2X s, vector2Y p),
                       (vector2X p + vector2X s, vector2Y p + vector2Y s), 
                       (vector2X p , vector2Y p + vector2Y s)]