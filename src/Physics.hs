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

updateState :: SF (Force, [Obstacle], Obstacle) (Pos, Vel, [Obstacle], Obstacle, GameState)
updateState = proc (f, obs, finish) -> do 
    rec
        acc <- identity -< f ^-^ rappelingForce ^-^ dragForce        
        vel <- integral -< acc 
        pos <- integral -< if state == Finished then zeroVector else vel
        rappelingForce <- identity -< (collisionForce ((pos, vel), obs))
        dragForce <- identity -< drag vel
        state <- identity -< checkFinish pos finish
    returnA -< (pos, vel, obs, finish, state) 
    


parseDirection :: Float -> Event [Direction] -> Force
parseDirection c dirs = if isEvent dirs then c *^ (mergeDirections $ fromEvent dirs) else zeroVector


mergeDirections :: [Direction] -> Force
mergeDirections dirs = if norm force > 0 then normalize force else zeroVector where
    force = foldl (^+^) zeroVector (map (\dir -> case dir of
        Types.None -> zeroVector
        Types.Right -> vector2 1.0 0.0
        Types.Left -> vector2 (-1.0) 0.0
        Types.Up -> vector2 0.0 1.0
        Types.Down -> vector2 0.0 (-1.0)) dirs)

simulate :: SF (Event [Direction], ([Obstacle], Obstacle)) (Pos, Vel, [Obstacle], Obstacle, GameState)
simulate = arrPrim (\(dir, (obs, finish)) ->  (parseDirection 250 dir, obs, finish)) >>> updateState
collisionForce :: ((Pos, Vel) , [Obstacle]) -> Force
collisionForce ((p,v), obs) = (checkCollisions' p v obs) *^ v

checkCollisions' :: Pos -> Vel -> [Obstacle] -> Float
checkCollisions' p _ obs = maximum $ map (checkCollision p) obs

checkCollision :: Pos -> Obstacle -> Float
checkCollision p (Obstacle oPos oSize sf') = if (vector2X p) >= (vector2X oPos) && 
                                           (vector2X p) <= (vector2X oPos + vector2X oSize) &&
                                           (vector2Y p) >= (vector2Y oPos) && 
                                           (vector2Y p) <= (vector2Y oPos + vector2Y oSize) then sf' else 0.0

checkFinish :: Pos -> Obstacle -> GameState
checkFinish p obs = if (checkCollision p obs) > 0 then Finished else Running
