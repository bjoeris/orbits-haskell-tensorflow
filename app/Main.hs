{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Control.Lens ((^.), (.~), (%~), (&), ix, over, mapping)
import Control.Monad (forM, forM_, when, foldM, join, (>>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.Vector.Lens (toVectorOf)
import Data.Yaml (FromJSON(..), (.:))
import qualified Data.Yaml as Y
import qualified Data.Csv as Csv
import Linear.V3 (V3, _x, _y, _z)
import Numeric.Units.Dimensional ((*~))
import qualified Numeric.Units.Dimensional as D
import System.Exit (exitFailure)
import System.IO (withFile, IOMode(WriteMode))
import TensorFlow.Core as TF

import Orbits.System (System, name, bodies, names, time, velocity, position)
import Orbits.Units (simTime, simEnergy, simVelocity, simLength, day, year, inUnit)
import Orbits.Simulation (getBodies, getEnergy, doStep)
import Orbits.Simulation.TensorFlow (createSimulation)

-- | Save the history of a single body to a CSV file
saveBodyHistory :: (Floating a, Csv.ToField a)
                => FilePath   -- ^ Path to CSV file to save. This file will be overwritten if it exists.
                -> Text       -- ^ Name of body
                -> Int        -- ^ Index of body in each system system of the history.
                              --   This must be the same for each system.
                -> [System a] -- ^ System history.
                -> IO ()
saveBodyHistory fileName bodyName bodyId history =
  withFile fileName WriteMode $ \file ->
    forM_ history $ \sys ->
      let t = sys^.time.inUnit simTime
          body = (sys^.bodies) ! bodyId
          position' = body^.position.mapping (inUnit simLength)
          velocity' = body^.velocity.mapping (inUnit simVelocity)
      in LBS.hPut file $ Csv.encode
        [( t,  position'^._x, position'^._y, position'^._z
          , velocity'^._x, velocity'^._y, velocity'^._z)]

main :: IO ()
main = TF.runSession $ do
    system0 <- liftIO $
      Y.decodeFileEither "solar_system.yaml" >>=
      either (\err -> putStrLn (Y.prettyPrintParseException err) >> exitFailure) return

    sim <- TF.build $ createSimulation $ system0^.bodies

    let dt = 0.1 *~ day
        outputPeriod = 10 *~ day
        totalTime = 10 *~ year
        totalSteps = (totalTime D./ dt)^.inUnit D.one
        stepsPerOutput = ceiling $ (outputPeriod D./ dt)^.inUnit D.one
        numOutputs = ceiling $ totalSteps / fromIntegral stepsPerOutput
        printEnergy system energy =
          liftIO $ putStrLn $
          "energy at time " ++ D.showIn simTime (system^.time) ++
          " = " ++ D.showIn simEnergy energy

    energy0 <- sim^.getEnergy
    printEnergy system0 energy0


    systems <- forM ([0..numOutputs] :: [Int]) $ \i -> do
      forM_ ([0..stepsPerOutput] :: [Int]) $ \j -> (sim^.doStep) dt
      newBodies <- sim^.getBodies
      return $ system0 & time %~ (D.+ (fromIntegral (i*stepsPerOutput) *~ D.one) D.* dt)
        & bodies .~ newBodies

    let system1 = last systems
    energy1 <- sim^.getEnergy
    printEnergy system1 energy1

    liftIO $ (system0^.names) & V.imapM_ (\i name ->
      saveBodyHistory
        (T.unpack name ++ ".csv")
        name i (system0 : systems))
