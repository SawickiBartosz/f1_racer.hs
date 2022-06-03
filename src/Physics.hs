{-#OPTIONS -Wall #-}
{-# LANGUAGE Arrows #-}

module Physics where
import FRP.Yampa as Yampa
import Types
import Data.Vector2

drag :: Vel -> Force
drag zeroVector = zeroVector
drag v = (-c * (norm v)) *^ (normalize v)
            where c = 0.2

applyForce :: SF ((Time, Force), [Obstacle]) (Pos, Vel, [Obstacle])
applyForce = proc ((_,f), obs) -> do 
    rec
        acc <- identity -< f ^-^ rappelingForce ^-^ dragForce
        vel <- integral -< acc
        pos <- integral -< vel
        rappelingForce <- identity -< (checkCollisions ((pos, vel), obs))
        dragForce <- identity -< drag vel
    returnA -< (pos, vel, obs) 
    

parseDirection :: Float -> (Time, Event [Direction])  -> (Time, Force)
parseDirection c (t, dirs) = if isEvent dirs then (t, c *^ (mergeDirections $ fromEvent dirs)) else (t, zeroVector)

mergeDirections :: [Direction] -> Force
mergeDirections dirs = if norm force > 0 then normalize force else zeroVector where
    force = foldl (^+^) zeroVector (map (\dir -> case dir of
        Types.None -> zeroVector
        Types.Right -> vector2 1.0 0.0
        Types.Left -> vector2 (-1.0) 0.0
        Types.Up -> vector2 0.0 1.0
        Types.Down -> vector2 0.0 (-1.0)) dirs)

simulate :: SF ((Time, Event [Direction]), [Obstacle]) (Pos, Vel, [Obstacle])
simulate = first (arrPrim (parseDirection 250)) >>> applyForce

checkCollisions :: ((Pos, Vel) , [Obstacle]) -> Force
checkCollisions ((p,v), obs) = (checkCollision p v obs) *^ v

checkCollision :: Pos -> Vel -> [Obstacle] -> Float
checkCollision p _ obs = maximum $ map (\(Obstacle oPos oSize sf') -> if (vector2X p) >= (vector2X oPos) && 
                                           (vector2X p) <= (vector2X oPos + vector2X oSize) &&
                                           (vector2Y p) >= (vector2Y oPos) && 
                                           (vector2Y p) <= (vector2Y oPos + vector2Y oSize) then sf' else  0.0) obs