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

mainSF :: [Obstacle] -- ^ List of obstacles in the world
        -> Finish  -- ^ Finish element
        ->  SF (Event InputEvent) Picture 
mainSF obs finish' = dSwitch ((constant renderIntro) &&& parseInput) 
                           (\ctrls -> if Types.Num1 `elem` ctrls
                              then runGame obs finish' simulateSingle 
                              else runGame obs finish' simulateDouble)
-- | Signal Function of the game 
runGame :: [Obstacle] -> -- ^ List of obstacles in the world
            Finish ->    -- ^ Finish element
            SF (Event [Controls], ([Obstacle], Finish)) World -> -- ^ SF from inputs to World
            SF (Event InputEvent) Picture
runGame obs finish' simulation = (parseInput &&& (constant obs &&& constant finish')) >>>
                                  simulation >>> arr renderWorld

-- | Single player simulation 
simulateSingle :: SF (Event [Controls], ([Obstacle], Finish)) World
simulateSingle = switch (simulateWorld Types.P1 red) fromWorld

-- | Double player simulation 
simulateDouble :: SF (Event [Controls], ([Obstacle], Finish)) World
simulateDouble = ((switch (simulateWorld Types.P1 red) fromWorld) &&& (switch (simulateWorld Types.P2 blue) fromWorld)) >>> arr mergeWorlds

-- | Inner simulation function
simulateWorld :: PlayerType -- ^ determining player
                -> Color -- ^ color of rendered player's car
                -> SF (Event [Controls], ([Obstacle], Finish)) (World, Event World)
simulateWorld p color' = (first $ arr (controlToDirection p)) >>> (Physics.simulate &&& FRP.Yampa.time) >>> arr (makeWorld color')
