-- {-# LANGUAGE Arrows #-}
module Main where

import FRP.Yampa
import Physics
import PlayYampa
import Types
import Control.Concurrent
import Data.Vector2
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

main :: IO ()
main = do
  playYampa
      (InWindow "F1 Racer" (800, 600) (200, 200))
      white
      60
      mainSF

mainSF :: SF (Event InputEvent) Picture
mainSF = (FRP.Yampa.time &&& parseInput) >>> (Physics.simulate &&& FRP.Yampa.time) >>> arr makeWorld >>> arr renderWorld

parseInput :: SF (Event InputEvent) (Event Direction)
parseInput = arr $ \event ->
  case event of
    Event (G.EventKey (G.SpecialKey G.KeyUp) G.Down _ _) -> event `tag` Types.Up
    Event (G.EventKey (G.SpecialKey G.KeyDown) G.Down _ _) -> event `tag` Types.Down
    Event (G.EventKey (G.SpecialKey G.KeyLeft) G.Down _ _) -> event `tag` Types.Left
    Event (G.EventKey (G.SpecialKey G.KeyRight) G.Down _ _) -> event `tag` Types.Right
    _ -> event `tag` None

makeWorld :: ((Pos, Vel), Time) -> World
makeWorld ((p,v), t) = World ([Car p v red]) t

renderWorld :: World -> Picture
renderWorld (World cars t) = pictures $ (map renderCar cars) ++ [scale 0.1 0.1 (text $ show $ round t)]

renderCar :: Car -> Picture
renderCar (Car p v c) =  Color c $ pictures [translate (vector2X p) (vector2Y p) (ThickCircle 5 10), 
                                             translate 50 200 $ scale 0.1 0.1 (text $ show v)]