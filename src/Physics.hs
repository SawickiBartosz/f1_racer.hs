{-#OPTIONS -Wall #-}

module Physics where
import FRP.Yampa as Yampa
import Types
import Data.Vector2


reduceVel:: SF Vel Vel
reduceVel = loopPre (zeroVector) (arrPrim (\(v1, vprev) -> (vprev, v1 ^-^ 0.1 *^ vprev)))

-- sscan :: (b -> a -> b) -> b -> SF a b
-- reduceVel:: SF Vel Vel
-- reduceVel = sscan (\vprev vin -> if (norm vin) > 0.1 then vin ^-^ 0.9 *^ vprev else vin) zeroVector


updatePosVel :: SF Acc (Pos, Vel)
updatePosVel = integral >>> reduceVel >>> (integral &&& identity)

addForce :: (Time, Force) -> Acc
addForce (_,f) = f 

drag :: Vel -> Force
drag v = (-c * (norm v)) *^ (normalize v)
            where c = 0.1

addForce' :: Mass -> Force -> SF Acc Acc
addForce' m force = constant (force ^/ m)

stopForce :: Time -> SF Acc (Acc, Event Force)
stopForce t = identity &&& after t zeroVector

applyForce :: SF (Time, Force) (Pos, Vel)
applyForce = arr addForce >>> updatePosVel

parseDirection :: Float -> SF (Time, Event Direction) (Time, Force)
parseDirection c = arrPrim (\(t, dir) -> 
    (t, case dir of
        Event Types.None -> zeroVector
        NoEvent -> zeroVector
        Event Types.Right -> c *^ (vector2 100.0 0.0)
        Event Types.Left -> c *^ (vector2 (-100.0) 0.0)
        Event Types.Up -> c *^ (vector2 0.0 100.0)
        Event Types.Down -> c *^ (vector2 0.0 (-100.0))
        ))

simulate :: SF (Time, Event Direction) (Pos, Vel)
simulate = parseDirection 10 >>> applyForce 

