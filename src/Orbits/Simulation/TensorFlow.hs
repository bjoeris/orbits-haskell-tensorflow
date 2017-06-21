{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Orbits.Simulation
import Orbits.System
import Orbits.Units

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
      masses = TF.constant [fromIntegral numBodies]
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

  -- positionDiffs[*,i,j] = 3d vector from body j to body i (first axis is x/y/z)
  let positionDiffs = TF.expandDims positionsVar _2 `TF.sub`
                      TF.expandDims positionsVar _1

      -- distance2[i,j] = squared distance between body i and body j
      distance2 = TF.sum (TF.square positionDiffs) _0

      -- distance[i,j] = distance between body i and body j
      distance = TF.sqrt distance2

      -- unitDiffs[*,i,j] = unit vector pointing from body j toward body i
      unitDiffs = positionDiffs `TF.realDiv` TF.expandDims distance _0

      -- gravitational constant, as a TensorFlow scalar
      g = TF.scalar simGravitationalConstant

      -- accelerations'[*,i,j] = acceleration on body i due to the gravitational attraction of body j
      -- accelerations'[*,i,i] = NaN
      accelerations' = g `TF.mul` unitDiffs `TF.mul`
                       TF.expandDims (TF.expandDims masses _0) _2 `TF.realDiv`
                       TF.expandDims distance2 _0

      -- accelerations[*,i,j] = acceleration on body i due to the gravitational attraction of body j
      -- accelerations'[*,i,i] = (0,0,0)
      accelerations = TF.matrixSetDiag accelerations' (TF.zerosLike positionsVar)

      -- totalAcceleration[*,i] = total acceleration on body i due to the gravitational attraction to all other bodies
      totalAcceleration = TF.sum accelerations _1

      -- newVelocities[*,i] = velocity of body i after one time step
      -- this assumes constant acceleration over the time step
      newVelocities = velocitiesVar `TF.add` (timeDelta `TF.mul` totalAcceleration)

      -- newPositions[*,i] = position of body i after one time step
      newPositions = positionsVar `TF.add` (timeDelta `TF.mul` newVelocities)

      -- massProd[i,j] = (mass of body i) * (mass of body j)
      massProd = TF.expandDims masses _0 `TF.mul`
                 TF.expandDims masses _1

      -- kineticEnergies[i] = 2 * (kinetic energy of body i)
      kineticEnergies = masses `TF.mul` TF.sum (TF.square velocitiesVar) _0

      -- kineticEnergy = total kinetic energy in the system
      kineticEnergy = TF.scalar 0.5 `TF.mul` TF.sum kineticEnergies _0

      -- gravitationalPotentials'[i,j] = gravitational potential between bodies i and j
      -- gravitationalPotentials'[i,i] = NaN
      gravitationalPotentials' = g `TF.mul` massProd `TF.realDiv` distance

      -- gravitationalPotentials[i,j] = gravitational potential between bodies i and j
      -- gravitationalPotentials[i,i] = 0
      gravitationalPotentials = TF.matrixSetDiag gravitationalPotentials' (TF.zerosLike kineticEnergies)

      -- gravitationalPotential = total gravitational potential of the system
      gravitationalPotential = TF.scalar 0.5 `TF.mul`
                               TF.sum  gravitationalPotentials (int32Vector [0, 1])

      -- energyOutput = total energy of the system
      energy = kineticEnergy - gravitationalPotential

  -- Operation which stores the new position in the position variable
  updatePositionsOp <- TF.assign positionsVar newPositions

  -- Operation which stores the new velocity in the velocity variable
  updateVelocitiesOp <- TF.assign velocitiesVar newVelocities

  -- Operation which performs one simulation step
  stepOp <- TF.group (updatePositionsOp, updateVelocitiesOp)

  let feedTimeDelta dt = TF.feed timeDelta $ TF.encodeTensorData [] (V.singleton (dt^.inUnit simTime))
      getBodies = bodiesFromVecs <$>
                  TF.run (positionsOutput, velocitiesOutput, masses)
      getEnergy = view (from $ inUnit simEnergy) .
                  TF.unScalar <$>
                  TF.run energy
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


infixl 6 ^+, ^-
infixl 7 ^*, ^/

(^+) :: TF.OneOf `[Complex Double, Complex Float, Int16, Int32, Int64, Int8, Word16, Word8, Double, Float]` t
     => TF.Tensor v'1 t
     -> TF.Tensor v'2 t
     -> TF.Tensor TF.Build t
a ^+ b = a `TF.add` b

(^-) :: TF.OneOf `[Complex Double, Complex Float, Int16, Int32, Int64, Int8, Word16, Word8, Double, Float]` t
     => TF.Tensor v'1 t
     -> TF.Tensor v'2 t
     -> TF.Tensor TF.Build t
a ^- b = a `TF.sub` b

(^*) :: TF.OneOf `[Complex Double, Complex Float, Int16, Int32, Int64, Int8, Word16, Word8, Double, Float]` t
     => TF.Tensor v'1 t
     -> TF.Tensor v'2 t
     -> TF.Tensor TF.Build t
a ^* b = a `TF.mul` b

(^/) :: TF.OneOf `[Complex Double, Complex Float, Int16, Int32, Int64, Int8, Word16, Word8, Double, Float]` t
     => TF.Tensor v'1 t
     -> TF.Tensor v'2 t
     -> TF.Tensor TF.Build t
a ^/ b = a `TF.realDiv` b
