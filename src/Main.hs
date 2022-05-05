module Main where

import Control.Monad
import FRP.Yampa
import Physics

main :: IO ()
main = reactimate firstSample nextSamples consoleConsumer signalFunction

firstSample :: IO Car
firstSample = return spawnCar

nextSamples :: Bool -> IO (DTime, Maybe Car)
nextSamples _ = return (10, Nothing)

consoleConsumer :: Bool -> Car -> IO Bool
consoleConsumer _ x = do
  print x
  return False

signalFunction :: SF Car Car
signalFunction = arr (\car -> updatePosition $ updateVelocity car Up)
