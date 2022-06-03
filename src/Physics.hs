{-#OPTIONS -Wall #-}
{-# LANGUAGE Arrows #-}

module Physics where
import FRP.Yampa as Yampa
import Types
import Data.Vector2

drag :: Vel -> Force
drag zeroVector = zeroVector
drag v = (-c * (norm v)) *^ (normalize v)
            where c = 0.05

applyForce :: SF ((Time, Force), [Obstacle]) (Pos, Vel, [Obstacle])
applyForce = proc ((_,f), obs) -> do 
    rec
        acc <- identity -< f ^-^ rappelingForce ^-^ dragForce
        vel <- integral -< acc
        pos <- integral -< vel
        rappelingForce <- identity -< (checkCollisions ((pos, vel), obs))
        dragForce <- identity -< drag vel
    returnA -< (pos, vel, obs) 
    

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

simulate :: SF ((Time, Event Direction), [Obstacle]) (Pos, Vel, [Obstacle])
simulate = first (parseDirection 10) >>> applyForce

checkCollisions :: ((Pos, Vel) , [Obstacle]) -> Force
checkCollisions ((p,v), obs) = (checkCollision p v obs) *^ v

checkCollision :: Pos -> Vel -> [Obstacle] -> Float
checkCollision p _ obs = maximum $ map (\(Obstacle oPos oSize sf') -> if (vector2X p) >= (vector2X oPos) && 
                                           (vector2X p) <= (vector2X oPos + vector2X oSize) &&
                                           (vector2Y p) >= (vector2Y oPos) && 
                                           (vector2Y p) <= (vector2Y oPos + vector2Y oSize) then sf' else  0.0) obs