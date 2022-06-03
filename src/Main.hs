{-#OPTIONS -Wall #-}
{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa
import Physics
import PlayYampa
import Types
import Data.Vector2
import Data.Csv
import qualified Data.Vector as DV
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as S
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G


main :: IO ()
main = do
  csvData <- BL.readFile "world.csv"
  obs <- case ((decode NoHeader csvData) :: Either String (DV.Vector ParsableObstacle)) of
        Prelude.Left _ -> return []
        Prelude.Right x -> return $ DV.toList $ DV.map fromParsableObstacle x
  playYampa
      (InWindow "F1 Racer" (800, 600) (400, 300))
      white
      60
      (mainSF obs)

mainSF :: [Obstacle] ->  SF (Event InputEvent) Picture
mainSF obs = ((FRP.Yampa.time &&& parseInput) &&& (constant obs)) >>> (Physics.simulate &&& FRP.Yampa.time) >>> arr makeWorld >>> arr renderWorld

parseInput :: SF (Event InputEvent) (Event [Direction])
parseInput = loopPre (S.empty) (arr parseInput')

parseInput' :: (Event InputEvent, S.Set (G.Key)) ->  (Event [Direction], S.Set (G.Key))
parseInput' (input, set) = (keysSetToDirections $ inputToKeysSet input set, inputToKeysSet input set)

inputToKeysSet :: Event InputEvent -> S.Set (G.Key) -> S.Set (G.Key)
inputToKeysSet (Event (G.EventKey k G.Down _ _)) set = S.insert k set
inputToKeysSet (Event (G.EventKey k G.Up _ _)) set = S.delete k set
inputToKeysSet (Event _) set = set
inputToKeysSet NoEvent set = set

keysSetToDirections :: S.Set (G.Key) -> Event [Direction]
keysSetToDirections set = catEvents $ map (\k -> 
  case k of
    (G.SpecialKey G.KeyDown) -> Event Types.Down
    (G.SpecialKey G.KeyUp) -> Event Types.Up
    (G.SpecialKey G.KeyLeft) -> Event Types.Left
    (G.SpecialKey G.KeyRight) -> Event Types.Right
    _ -> Event Types.None) (S.toList set)


makeWorld :: ((Pos, Vel, [Obstacle]), Time) -> World
makeWorld ((p,v, obs), t) = World ([Car p v red]) obs t

renderWorld :: World -> Picture
renderWorld (World cars' obs t) = pictures $ (map renderCar cars') ++ (map renderObstacle obs) ++ [scale 0.1 0.1 (text $ show $ ((round t) :: Integer))]

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