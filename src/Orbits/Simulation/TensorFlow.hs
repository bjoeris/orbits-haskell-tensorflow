{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Orbits.Simulation.TensorFlow (createSimulation) where

import Control.Lens
import Data.Int (Int32, Int64)
import Data.Vector (Vector)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Numeric.Units.Dimensional as D
import qualified TensorFlow.Core as TF
import qualified TensorFlow.Ops as TF
  ( scalar, vector, placeholder, initializedVariable, constant )
import qualified TensorFlow.GenOps.Core as TF
  ( sum, mul, sub, squeeze, matrixSetDiag, neg, square, expandDims, assign, add, zerosLike, realDiv, concat, sqrt, slice, transpose, print' )
import qualified TensorFlow.Types as TF (ListOf((:/), Nil))

import Orbits.Simulation (Simulation(Simulation))
import Orbits.System (Body(Body), position, velocity, mass, bodiesFromVecs)
import Orbits.Units (inUnit, simEnergy, simTime, simLength, simVelocity, simMass, simGravitationalConstant)

-- | Helper for creating a constant Int32 scalar.
int32Scalar :: Int32 -> TF.Tensor TF.Build Int32
int32Scalar = TF.scalar

-- | Helper for creating a constant Int32 rank-1 tensor.
int32Vector :: [Int32] -> TF.Tensor TF.Build Int32
int32Vector = TF.vector

-- | Build the TensorFlow graph, and return the simulation object to control it
createSimulation :: Vector (Body Double)
                 -> TF.Build (Simulation TF.Session Double)
createSimulation initialBodies = do
  let numBodies = V.length initialBodies
      initialPositions = TF.constant [fromIntegral numBodies, 3]
        (initialBodies & toListOf (traverse.position.traverse.inUnit simLength))
      initialVelocities = TF.constant [fromIntegral numBodies, 3]
        (initialBodies & toListOf (traverse.velocity.traverse.inUnit simVelocity))
      masses = TF.constant [1, fromIntegral numBodies]
        (initialBodies & toListOf (traverse.mass.inUnit simMass))
  positionsVar <- TF.initializedVariable (TF.transpose initialPositions (int32Vector [1, 0]))
  velocitiesVar <- TF.initializedVariable (TF.transpose initialVelocities (int32Vector [1, 0]))
  positionsOutput <- TF.render $ TF.transpose positionsVar (int32Vector [1, 0])
  velocitiesOutput <- TF.render $ TF.transpose velocitiesVar (int32Vector [1, 0])

  timeDelta <- TF.placeholder []

  let log name tensor = return tensor
        -- TF.print' (set (TF.opAttr "summarize") (100 :: Int64) .
        --            set (TF.opAttr "message") (name :: BS.ByteString))
        -- tensor
        -- (tensor TF.:/ TF.Nil)

      -- (dx, dy, dz) for each pair
  positionDiffs <- log "positionDiffs" $ TF.expandDims positionsVar (int32Scalar 2) `TF.sub`
                   TF.expandDims positionsVar (int32Scalar 1)

      -- dx^2 + dy^2 + dz^2 for each pair
  distance2 <- log "distance2" $ TF.sum (TF.square positionDiffs) (int32Scalar 0)
  distance <- log "distance" $ TF.sqrt distance2

      -- unit vector pointing between each pair
  unitDiffs <- log "unitDiffs" $ positionDiffs `TF.realDiv` TF.expandDims distance (int32Scalar 0)

  g <- log "g" $ TF.scalar simGravitationalConstant

  accelerations' <- log "accelerations'" $ g `TF.mul` unitDiffs `TF.mul`
                       TF.expandDims masses (int32Scalar 2) `TF.realDiv`
                       TF.expandDims distance2 (int32Scalar 0)

  accelerations <- log "accelerations" $ TF.matrixSetDiag accelerations' (TF.zerosLike positionsVar)

  totalAcceleration <- log "totalAcceleration" $ TF.sum accelerations (int32Scalar 1)

  newVelocities <- log "newVelocities" $ velocitiesVar `TF.add` (timeDelta `TF.mul` totalAcceleration)
  newPositions <- log "newPositions" $ positionsVar `TF.add` (timeDelta `TF.mul` newVelocities)

  -- newBodies <- TF.render $ TF.concat (TF.scalar 0)
  --              [ newPositions, newVelocities, massesVar ]
  updatePositions <- TF.assign positionsVar newPositions
  updateVelocities <- TF.assign velocitiesVar newVelocities
  stepOp <- TF.group (updatePositions, updateVelocities)

  massProd <- log "massProd" $ TF.expandDims masses (int32Scalar 1) `TF.mul`
                 TF.expandDims masses (int32Scalar 2)

  kineticEnergies <- log "kineticEnergies" $ masses `TF.mul` TF.sum (TF.square velocitiesVar) (int32Scalar 0)
  gravitationalPotentials <- log "gravitationalPotentials" $ TF.neg $ g `TF.mul` massProd `TF.realDiv` distance
  energies <- log "energies" $ TF.scalar 0.5 `TF.mul` TF.matrixSetDiag gravitationalPotentials kineticEnergies
  energyOutput <- TF.render $ TF.squeeze (TF.sum energies (int32Vector [1, 2]))
  energyOutput <- log "energyOutput" energyOutput

  let feedTimeDelta dt = TF.feed timeDelta $ TF.encodeTensorData [] (V.singleton (dt^.inUnit simTime))
      -- feedBodies bodies = TF.feed bodiesInput $ bodiesToTensor bodies

      -- setBodies bodies = TF.runWithFeeds_ [feedBodies bodies] setBodiesOp
      getBodies :: TF.Session (Vector (Body Double))
      getBodies = bodiesFromVecs <$>
        TF.run (positionsOutput, velocitiesOutput, masses)
      getEnergy = view (from $ inUnit simEnergy) .
                  TF.unScalar <$>
                  TF.run energyOutput
      step dt = TF.runWithFeeds_ [feedTimeDelta dt] stepOp

  return $ Simulation getBodies getEnergy step
