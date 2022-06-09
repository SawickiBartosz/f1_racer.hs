{-#OPTIONS -Wall #-}
{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa
import Physics
import PlayYampa
import Input
import Types
import Render
import World
import Data.Csv
import qualified Data.Vector as DV
import qualified Data.ByteString.Lazy as BL
import Graphics.Gloss

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

mainSF :: [Obstacle] -> Finish ->  SF (Event InputEvent) Picture
mainSF obs finish' = dSwitch ((constant renderIntro) &&& parseInput) 
                           (\ctrls -> if Types.P1 `elem` ctrls
                              then runGame obs finish' simulateSingle 
                              else runGame obs finish' simulateDouble)

runGame :: [Obstacle] -> 
            Finish ->   
            SF (Event [Controls], ([Obstacle], Finish)) World ->
            SF (Event InputEvent) Picture
runGame obs finish' simulation = (parseInput &&& (constant obs &&& constant finish')) >>>
                                  simulation >>> arr renderWorld

simulateSingle :: SF (Event [Controls], ([Obstacle], Finish)) World
simulateSingle = switch (simulateWorld Types.P1 red) fromWorld

simulateDouble :: SF (Event [Controls], ([Obstacle], Finish)) World
simulateDouble = ((switch (simulateWorld Types.P1 red) fromWorld) &&& (switch (simulateWorld Types.P2 blue) fromWorld)) >>> arr mergeWorlds

simulateWorld :: Controls -> Color -> SF (Event [Controls], ([Obstacle], Finish)) (World, Event World)
simulateWorld c col = arr (translateControls c) >>> (Physics.simulate &&& FRP.Yampa.time) >>> arr (makeWorld col)


