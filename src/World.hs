module World where

import FRP.Yampa
import Types
import Graphics.Gloss

-- | make constant SF from World, used to snapshot state afertr finish
fromWorld :: World -> SF a World
fromWorld (World cars' obs fin t s) = constant (World cars' obs fin t s)

-- | Make World from individual components
-- Always returns World and if game finished also Event of World
makeWorld :: Color -- ^ color of player
            -> ((Pos, Vel, [Obstacle], Finish, GameState), Time)
            -> (World, Event World)
makeWorld c ((p,v, obs, fin, Running), t) = (World ([Car p v c]) obs fin [t] Running, NoEvent)
makeWorld c ((p,v, obs, fin, Finished), t) = (World ([Car p v c]) obs fin [t] Finished, Event (World ([Car p v c]) obs fin [t] Finished))

-- | Join worlds of each player into one
mergeWorlds :: (World, World) -> World
mergeWorlds (World [Car p1 v1 c1] obs fin [t1] Running, World [Car p2 v2 c2] _ _ [t2] Running)   = World ([Car p1 v1 c1, Car p2 v2 c2]) obs fin [t1,t2] Running
mergeWorlds (World [Car p1 v1 c1] obs fin [t1] Finished, World [Car p2 v2 c2] _ _ [t2] Finished) = World ([Car p1 v1 c1, Car p2 v2 c2]) obs fin [t1,t2] Finished
mergeWorlds (World [Car p1 v1 c1] obs fin [t1] _, World [Car p2 v2 c2] _ _ [t2] _)               = World ([Car p1 v1 c1, Car p2 v2 c2]) obs fin [t1,t2] Running
