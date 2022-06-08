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
      (InWindow "F1 Racer" (1920, 1080) (800, 600))
      white
      60
      (mainSF (init obs) (last obs))

runGame :: [Obstacle] -> 
            Obstacle ->   
            SF (Event [Controls], ([Obstacle], Obstacle)) World ->
            SF (Event InputEvent) Picture
runGame obs finish simulation = (parseInput &&& (constant obs &&& constant finish)) >>>
  simulation >>> arr renderWorld

simulateSingle :: SF (Event [Controls], ([Obstacle], Obstacle)) World
simulateSingle = arr (translateControls Types.P1) >>> (Physics.simulate &&& FRP.Yampa.time) >>> arr makeWorld

simulateDouble :: SF (Event [Controls], ([Obstacle], Obstacle)) World
simulateDouble = ((arr (translateControls Types.P1) >>> (Physics.simulate &&& FRP.Yampa.time)) &&&
                  (arr (translateControls Types.P2) >>> (Physics.simulate &&& FRP.Yampa.time))) >>> arr makeWorld2


mainSF :: [Obstacle] -> Obstacle ->  SF (Event InputEvent) Picture
mainSF obs finish = dSwitch ((constant renderIntro) &&& parseInput) 
                           (\ctrls -> if Types.P1 `elem` ctrls
                              then runGame obs finish simulateSingle 
                              else runGame obs finish simulateDouble)

translateControls :: Controls -> (Event [Controls], ([Obstacle], Obstacle)) -> (Event [Direction], ([Obstacle], Obstacle))
translateControls p (ctrls, (obs, fin)) = ((controlToDirection p ctrls), (obs, fin))

parseInput :: SF (Event InputEvent) (Event [Controls])
parseInput = loopPre (S.empty) (arr parseInput')

parseInput' :: (Event InputEvent, S.Set (G.Key)) ->  (Event [Controls], S.Set (G.Key))
parseInput' (input, set) = (keysSetToControls $ inputToKeysSet input set, inputToKeysSet input set)

inputToKeysSet :: Event InputEvent -> S.Set (G.Key) -> S.Set (G.Key)
inputToKeysSet (Event (G.EventKey k G.Down _ _)) set = S.insert k set
inputToKeysSet (Event (G.EventKey k G.Up _ _)) set = S.delete k set
inputToKeysSet (Event _) set = set
inputToKeysSet NoEvent set = set

keysSetToControls :: S.Set (G.Key) -> Event [Controls]
keysSetToControls set = catEvents $ map (\k -> 
  case k of
    (G.SpecialKey G.KeyDown)  -> Event Types.KeyDown
    (G.SpecialKey G.KeyUp)    -> Event Types.KeyUp
    (G.SpecialKey G.KeyLeft)  -> Event Types.KeyLeft
    (G.SpecialKey G.KeyRight) -> Event Types.KeyRight
    (G.SpecialKey G.KeySpace) -> Event Types.Space
    (G.Char 'w')              -> Event Types.W
    (G.Char 'r')              -> Event Types.R
    (G.Char 's')              -> Event Types.S
    (G.Char 'a')              -> Event Types.A
    (G.Char 'd')              -> Event Types.D
    (G.Char '1')              -> Event Types.P1
    (G.Char '2')              -> Event Types.P2
    _                         -> Event Types.NoControl)
  (S.toList set)


makeWorld :: ((Pos, Vel, [Obstacle], Obstacle, GameState), Time) -> World
makeWorld ((p,v, obs, fin, s), t) = World ([Car p v red]) obs fin t s

makeWorld2 :: (((Pos, Vel, [Obstacle], Obstacle, GameState), Time), ((Pos, Vel, [Obstacle], Obstacle, GameState), Time)) -> World
makeWorld2 (((p1,v1, obs, fin, s1), t), ((p2,v2, _, _, s2), _)) = World ([Car p1 v1 red, Car p2 v2 blue]) obs fin t merged_state 
                                      where merged_state = minimum [s1, s2]

renderIntro :: Picture
renderIntro = translate (-200) 0 $ scale 0.2 0.2 $ text "Press any key to start the game"

renderWorld :: World -> Picture
renderWorld (World cars' obs fin t Running) = translate 0 (-200) $ pictures $ (map renderCar cars') ++ 
                                                         (map renderObstacle obs) ++
                                                         [renderFinish fin] ++ 
                                                         [scale 0.1 0.1 (text $ show $ ((round t) :: Integer))]
renderWorld (World _ _ _ t Finished) = pictures $ [translate (-100) 0 $ scale 0.4 0.4 $ text $ "Finished!"] ++
                                                  [translate (-200) (-50) $ scale 0.3 0.3 $ text $ "Your time is: " ++ (show t)]

renderCar :: Car -> Picture
renderCar (Car p v c) =  Color c $ translate (vector2X p) (vector2Y p) (ThickCircle 4 7)

renderObstacle :: Obstacle -> Picture
renderObstacle (Obstacle p s _) = polygon $ pathFromPosSize p s

renderFinish :: Obstacle -> Picture
renderFinish (Obstacle p s _) = Color red $ polygon $ pathFromPosSize p s

pathFromPosSize :: Pos -> Vector2 Float -> Path
pathFromPosSize p s = [(vector2X p, vector2Y p), 
                       (vector2X p + vector2X s, vector2Y p),
                       (vector2X p + vector2X s, vector2Y p + vector2Y s), 
                       (vector2X p , vector2Y p + vector2Y s)]