{-#OPTIONS -Wall #-}
{-# LANGUAGE Arrows #-}

module Physics where
import FRP.Yampa as Yampa
import Types
import Data.Vector2

-- | calculate drag force applying to object with given velocity
drag :: Vel  
        -> Force
drag zeroVector = zeroVector
drag v = (-c * (norm v)) *^ (normalize v)
            where c = 0.2

-- | SF utilising input force, obstacles and finish point returning components of a world
updateState :: SF (Force, [Obstacle], Finish) (Pos, Vel, [Obstacle], Finish, GameState)
updateState = proc (f, obs, finish') -> do 
    rec
        acc <- identity -< f ^-^ rappelingForce ^-^ dragForce        
        vel <- integral -< acc 
        pos <- integral -< if state == Finished then zeroVector else vel
        rappelingForce <- identity -< (collisionForce ((pos, vel), obs))
        dragForce <- identity -< drag vel
        state <- identity -< checkFinish pos finish'
    returnA -< (pos, vel, obs, finish', state) 
    
-- | translate input events to force 
parseDirection :: Float -- ^ constant to multiply force by, the bigger, the faster objects in game seem
                -> Event [Direction] -- ^ incoming directions of movement
                -> Force
parseDirection c dirs = if isEvent dirs then c *^ (mergeDirections $ fromEvent dirs) else zeroVector

-- |translate directions to nomralized vector of force
mergeDirections :: [Direction] -> Force
mergeDirections dirs = if norm force > 0 then normalize force else zeroVector where
    force = foldl (^+^) zeroVector (map (\dir -> case dir of
        Types.None -> zeroVector
        Types.Right -> vector2 1.0 0.0
        Types.Left -> vector2 (-1.0) 0.0
        Types.Up -> vector2 0.0 1.0
        Types.Down -> vector2 0.0 (-1.0)) dirs)

-- | main physics simulation
simulate :: SF (Event [Direction], ([Obstacle], Finish)) (Pos, Vel, [Obstacle], Finish, GameState)
simulate = arrPrim (\(dir, (obs, finish')) ->  (parseDirection 250 dir, obs, finish')) >>> updateState

-- | calculate force resulting from collisions with obstacles
collisionForce :: ((Pos, Vel) -- ^ position and velocity of an object
                 , [Obstacle]) -- ^ list of obstacles to check collision with
                 -> Force -- ^ force resulting from hit
collisionForce ((p,v), obs) = (checkCollisions' p v obs) *^ v

-- | inner function to calculate collision force
checkCollisions' :: Pos -> Vel -> [Obstacle] -> Float
checkCollisions' p _ obs = maximum $ map (checkCollision p) obs

-- | check collision with single obstacle
checkCollision :: Pos -> Obstacle -> Float
checkCollision p (Obstacle oPos oSize sf') = if (vector2X p) >= (vector2X oPos) && 
                                           (vector2X p) <= (vector2X oPos + vector2X oSize) &&
                                           (vector2Y p) >= (vector2Y oPos) && 
                                           (vector2Y p) <= (vector2Y oPos + vector2Y oSize) then sf' else 0.0

-- | check if game is fininshed
checkFinish :: Pos -> Finish -> GameState
checkFinish p obs = if (checkCollision p obs) > 0 then Finished else Running
