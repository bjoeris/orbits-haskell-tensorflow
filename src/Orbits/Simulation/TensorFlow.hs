{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Orbits.Simulation.TensorFlow (createSimulation) where

import Control.Lens hiding (_1, _2)
import Data.Int (Int32, Int64)
import Data.Vector (Vector)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Numeric.Units.Dimensional as D
import qualified TensorFlow.Core as TF
import qualified TensorFlow.Ops as TF
  ( scalar, vector, placeholder, initializedVariable, constant )
import qualified TensorFlow.GenOps.Core as TF
  ( sum, mul, sub, squeeze, matrixSetDiag, neg, square, expandDims, assign, add
  , zerosLike, realDiv, concat, sqrt, slice, transpose, print' )
import qualified TensorFlow.Types as TF (ListOf((:/), Nil))

import TensorFlow.Operators
import Orbits.Simulation
import Orbits.System
import Orbits.Units

data State a = State
  { _positions :: TF.Tensor TF.Value a  -- ^ Positions. Shape: [3, n]
  , _velocities :: TF.Tensor TF.Value a -- ^ Velocities. Shape: [3, n]
  , _masses :: TF.Tensor TF.Value a     -- ^ Masses. Shape: [n]
  }

makeLenses ''State

-- | Build a TensorFlow graph to compute the new velocities.
buildNewPositions :: TF.Tensor v'0 Double -- ^ Length of time step. Scalar.
                  -> State Double  -- ^ current state of the system
                  -> TF.Build (TF.Tensor TF.Value Double) -- ^ new velocities. Shape: [3, n].
buildNewPositions timeDelta state =
  TF.render $ state^.positions ^+ timeDelta ^* state^.velocities

-- | Build a TensorFlow graph to compute the new velocities.
buildNewVelocities :: TF.Tensor v'0 Double -- ^ Length of time step. Scalar.
                   -> State Double
                   -> TF.Build (TF.Tensor TF.Value Double) -- ^ new velocities. Shape: [3, n].
buildNewVelocities timeDelta state = do
  -- distance[i,j] = distance from body i to body j
  -- distance2[i,j] = squared distance from body i to body j
  -- positionDiffs[*,i,j] = vector from body j to body i
  (distance, distance2, positionDiffs) <- buildDistance state

      -- unitDiffs[*,i,j] = unit vector pointing from body j toward body i
  let unitDiffs = positionDiffs ^/ distance

      -- masses, expanded to shape [n,1], to line up with the shape of distance2
      masses' = TF.expandDims (state^.masses) _1

      -- accelerations'[*,i,j] = acceleration on body i due to the gravitational attraction of body j
      -- accelerations'[*,i,i] = NaN
      accelerations' =
        unitDiffs ^*
        TF.scalar simGravitationalConstant ^*
        masses' ^/
        distance2

      -- accelerations[*,i,j] = acceleration on body i due to the gravitational attraction of body j
      -- accelerations'[*,i,i] = (0,0,0)
      accelerations = TF.matrixSetDiag accelerations' (TF.zerosLike (state^.positions))

      -- totalAcceleration[*,i] = total acceleration on body i due to the gravitational attraction to all other bodies
      totalAcceleration = TF.sum accelerations _1

  TF.render $ state^.velocities ^+ timeDelta ^* totalAcceleration


-- | Build a TensorFlow graph to compute the energy of the system
buildEnergy :: State Double
            -> TF.Build (TF.Tensor TF.Value Double)
buildEnergy state = do
  -- distance[i,j] = distance from body i to body j
  (distance, _, _) <- buildDistance state

      -- kineticEnergy = total kinetic energy in the system
  let kineticEnergy =
        TF.scalar 0.5 ^* TF.sum kineticEnergies _0

      -- kineticEnergies[i] = 2 * (kinetic energy of body i)
      kineticEnergies =
        state^.masses ^* TF.sum (TF.square (state^.velocities)) _0

      -- gravitationalPotential = total gravitational potential of the system
      gravitationalPotential =
        TF.scalar 0.5 ^* TF.sum  gravitationalPotentials (int32Vector [0, 1])

      -- gravitationalPotentials[i,j] = gravitational potential between bodies i and j
      -- gravitationalPotentials[i,i] = 0
      gravitationalPotentials =
        TF.matrixSetDiag gravitationalPotentials' (TF.zerosLike kineticEnergies)

      -- gravitationalPotentials'[i,j] = gravitational potential between bodies i and j
      -- gravitationalPotentials'[i,i] = NaN
      gravitationalPotentials' =
        TF.scalar simGravitationalConstant ^* massProd ^/ distance

      -- massProd[i,j] = (mass of body i) * (mass of body j)
      massProd =
        TF.expandDims (state^.masses) _0 ^* -- masses, in shape [1,n]
        TF.expandDims (state^.masses) _1    -- masses, in shape [n,1]
  TF.render $ kineticEnergy - gravitationalPotential

-- | Build TensorFlow graph to compute the distance between the bodies, and related tensors.
buildDistance :: State Double
              -> TF.Build ( TF.Tensor TF.Value Double
                          , TF.Tensor TF.Value Double
                          , TF.Tensor TF.Value Double )
              -- ^ returns (distance, distance^2, positionDiffs),
              --   where distance[i,j] is the distance between body i and body j
              --   and positionDiffs[*,i,j] is the vector from body j to body i
buildDistance state = do
  -- positionDiffs[*,i,j] = 3d vector from body j to body i (first axis is x/y/z)
  positionDiffs <- TF.render (TF.expandDims (state^.positions) _2 ^-  -- positions, in shape [*,n,1]
                              TF.expandDims (state^.positions) _1)    -- positions, in shape [*,1,n]

  -- distance2[i,j] = squared distance between body i and body j
  distance2 <- TF.render $ TF.sum (TF.square positionDiffs) _0

  -- distance[i,j] = distance between body i and body j
  distance <- TF.render $ TF.sqrt distance2

  return (distance, distance2, positionDiffs)


-- | Build the TensorFlow graph, and return the simulation object to control it
createSimulation :: Vector (Body Double)
                 -> TF.Build (Simulation TF.Session Double)
createSimulation initialBodies = do
  let numBodies = V.length initialBodies

      -- initialPositions[i,*] = initial position of body i. Note that the last dimension is the x/y/z channel.
      initialPositions = TF.constant [fromIntegral numBodies, 3]
        (initialBodies & toListOf (traverse.position.traverse.inUnit simLength))

      -- initialVelocities[i,*] = initial velocity of body i.
      initialVelocities = TF.constant [fromIntegral numBodies, 3]
        (initialBodies & toListOf (traverse.velocity.traverse.inUnit simVelocity))

  -- masses[i] = mass of body i.
  masses <- TF.render $ TF.constant [fromIntegral numBodies]
    (initialBodies & toListOf (traverse.mass.inUnit simMass))

  -- positionsVar[*,i] = current position of body i (mutable variable).
  -- Note that the first dimension is the x/y/z channel.
  positionsVar <- TF.initializedVariable (TF.transpose initialPositions (int32Vector [1, 0]))

  -- velocitiesVar[*,i] = current velocity of body i (mutable variable).
  velocitiesVar <- TF.initializedVariable (TF.transpose initialVelocities (int32Vector [1, 0]))

  -- positionsOutput[i,*] = current positions of the body i.
  -- Note that the last dimension is the x/y/z channel.
  positionsOutput <- TF.render $ TF.transpose positionsVar (int32Vector [1, 0])

  -- velocitiesOutput[i,*] = current velocity of body i.
  velocitiesOutput <- TF.render $ TF.transpose velocitiesVar (int32Vector [1, 0])

  -- Time step size. Must be fed in when doing the step
  timeDelta <- TF.placeholder []

  let state = State
        { _positions = TF.value positionsVar
        , _velocities = TF.value velocitiesVar
        , _masses = masses
        }

  -- new positions, obtained by moving each body at the current velocity for 'timeDelta' amount of time
  newPositions <- buildNewPositions timeDelta state

  -- new velocities, computed *at the new positions* by adding the gravitational forces from all other bodies
  newVelocities <- buildNewVelocities timeDelta (state & positions .~ newPositions)

  -- current energy of the system
  energy <- buildEnergy state

  -- Operation which stores the new position in the position variable
  updatePositionsOp <- TF.assign positionsVar newPositions

  -- Operation which stores the new velocity in the velocity variable
  updateVelocitiesOp <- TF.assign velocitiesVar newVelocities

  -- Operation which performs one simulation step
  stepOp <- TF.group (updatePositionsOp, updateVelocitiesOp)

      -- Feed a 'Quantity' of time into the timeDelta placeholder
  let feedTimeDelta dt = TF.feed timeDelta $ TF.encodeTensorData [] (V.singleton (dt^.inUnit simTime))

      -- get the positions, velocities and masses of the bodies as flat 'Vector Double's,
      -- and convert these into a 'Vector (Body Double)'
      getBodies = bodiesFromVecs <$>
                  TF.run (positionsOutput, velocitiesOutput, masses)

      -- get the energy, and convert it into a 'Quantity'
      getEnergy = view (from $ inUnit simEnergy) .
                  TF.unScalar <$>
                  TF.run energy

      -- advance the simulation by 1 time step
      step dt = TF.runWithFeeds_ [feedTimeDelta dt] stepOp

  return $ Simulation getBodies getEnergy step

-- | Helper for creating a constant Int32 scalar.
int32Scalar :: Int32 -> TF.Tensor TF.Build Int32
int32Scalar = TF.scalar

-- | Helper for creating a constant Int32 rank-1 tensor.
int32Vector :: [Int32] -> TF.Tensor TF.Build Int32
int32Vector = TF.vector

-- | Convenient shorthand for 'int32Scalar 0'
_0 :: TF.Tensor TF.Build Int32
_0 = TF.scalar 0

-- | Convenient shorthand for 'int32Scalar 1'
_1 :: TF.Tensor TF.Build Int32
_1 = TF.scalar 1

-- | Convenient shorthand for 'int32Scalar 2'
_2 :: TF.Tensor TF.Build Int32
_2 = TF.scalar 2
