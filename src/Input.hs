module Input where

import FRP.Yampa
import Types
import qualified Data.Set as S
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

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

translateControls :: Controls -> (Event [Controls], ([Obstacle], Finish)) -> (Event [Direction], ([Obstacle], Finish))
translateControls p (ctrls, (obs, fin)) = ((controlToDirection p ctrls), (obs, fin))
