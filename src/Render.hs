module Render where

import FRP.Yampa
import Physics
import PlayYampa
import Input
import Types
import Graphics.Gloss
import Data.Vector2

renderIntro :: Picture
renderIntro = pictures $ [translate (-200) 0     $ scale 0.2 0.2 $ text "Press any key to start mulitplayer",
                          translate (-180) (-40) $ scale 0.2 0.2 $ text "Press 1 to start singleplayer"]

renderWorld :: World -> Picture
renderWorld (World cars' obs fin _ Running) = translate 0 (-200) $ pictures $ (map renderCar cars') ++ 
                                                         (map renderObstacle obs) ++
                                                         [renderFinish fin] 
renderWorld (World [_] _ _ ts Finished) = pictures $ [translate (-100) 0 $ scale 0.4 0.4 $ text $ "Finished!"] ++
                                                  [translate (-200) (-50) $ scale 0.3 0.3 $ text $ "Your time is: " ++ (showTime $ head ts)]

renderWorld (World [_,_] _ _ ts Finished) = pictures $ [translate (-100) 0 $ scale 0.4 0.4 $ text $ "Finished!"] ++ (renderWinner ts)

renderCar :: Car -> Picture
renderCar (Car p v c) =  Color c $ translate (vector2X p) (vector2Y p) (ThickCircle 4 7)

renderWinner :: [Time] -> [Picture]
renderWinner [t1,t2]
  | t1 < t2 = [translate (-200) (-50) $ scale 0.3 0.3 $ Color red $ text $ "Player 1 won in " ++ showTime t1] ++
              [translate (-200) (-100) $ scale 0.3 0.3 $ Color blue $ text $ "Player 2 lost: " ++ showTime t2]
  | t1 > t2 = [translate (-200) (-50) $ scale 0.3 0.3 $ Color blue $ text $ "Player 2 won in " ++ showTime t2] ++
              [translate (-200) (-100) $ scale 0.3 0.3 $ Color red $ text $ "Player 1 lost: " ++ showTime t1]
  | otherwise = [translate (-200) (-50) $ scale 0.3 0.3 $ text $ "Draw in time: " ++ showTime t2]

renderObstacle :: Obstacle -> Picture
renderObstacle (Obstacle p s _) = polygon $ pathFromPosSize p s

renderFinish :: Obstacle -> Picture
renderFinish (Obstacle p s _) = Color red $ polygon $ pathFromPosSize p s

pathFromPosSize :: Pos -> Vector2 Float -> Path
pathFromPosSize p s = [(vector2X p, vector2Y p), 
                       (vector2X p + vector2X s, vector2Y p),
                       (vector2X p + vector2X s, vector2Y p + vector2Y s), 
                       (vector2X p , vector2Y p + vector2Y s)]

showTime :: Time -> String
showTime t = show $ truncate' t 3

truncate' :: Double -> Int -> Double
truncate' x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n