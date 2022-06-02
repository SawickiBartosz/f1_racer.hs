{-#OPTIONS -Wall #-}
{-# LANGUAGE Arrows #-}

module Physics where
import FRP.Yampa as Yampa
import Types
import Data.Vector2

drag :: Vel -> IO Force
drag zeroVector = return zeroVector
drag v = return $ (-c * (norm v)) *^ (normalize v)
            where c = 0.05

applyForce' :: SF ((Time, Force), IO [Obstacle]) ((Pos, Vel))
applyForce' = proc ((t,f), obs) -> do 
    rec
        acc <- identity -< mergeForces
        vel <- integral -< acc
        pos <- integral -< vel
        rappelingForce <- identity -< (checkCollisions ((pos, vel), obs))
        dragForce <- identity -< drag vel
    returnA -< (pos, vel) where
        mergeForces = f ^-^ rappelingForce ^-^ dragForce


mergeForces :: IO Force -> IO Force -> IO

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

simulate :: SF ((Time, Event Direction), IO [Obstacle]) (Pos, Vel)
simulate = first (parseDirection 10) >>> applyForce'

checkCollisions :: ((Pos, Vel) , IO [Obstacle]) -> IO Force
checkCollisions ((p,v), obs) = do
    slowingForce <- checkCollision p v obs 
    return $ slowingForce *^ v


checkCollision :: Pos -> Vel -> [Obstacle] -> Float
checkCollision p _ obs = maximum $ map (\(Obstacle oPos oSize sf) -> if (vector2X p) >= (vector2X oPos) && 
                                           (vector2X p) <= (vector2X oPos + vector2X oSize) &&
                                           (vector2Y p) >= (vector2Y oPos) && 
                                           (vector2Y p) <= (vector2Y oPos + vector2Y oSize) then  sf else  0.0) obs