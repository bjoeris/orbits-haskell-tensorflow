{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module Orbits.Units
  ( au
  , solarMass
  , simLength
  , simTime
  , simMass
  , simVelocity
  , simEnergy
  , gravitationalConstant
  , simGravitationalConstant
  , day
  , year
  , inUnit
  , fromUnit
  ) where

import Prelude ()

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI (year)
import Numeric.Units.Dimensional.UnitNames (atom)

import Control.Lens ((^.), Iso', iso, from)

-- | An Iso, for moving between a physical quantity and a raw number,
--   representing that physical quantity in some units
inUnit :: Fractional a => Unit m d a -> Iso' (Quantity d a) a
inUnit u = iso (/~ u) (*~ u)

fromUnit :: Fractional a => Unit m d a -> Iso' a (Quantity d a)
fromUnit u = from $ inUnit u

-- | Astronomical unit.
--   Approximate distance between Earth and Sol
au :: Floating a => Unit 'NonMetric DLength a
au = mkUnitR (atom "au" "au" "astronomical unit") 1.49597870700e+11 meter

-- | Mass of Sol.
solarMass :: Floating a => Unit 'NonMetric DMass a
solarMass = mkUnitR (atom "mS" "mS" "solar mass") 1.98855e+30 (kilo gram)

-- | Unit of length used for the simulation
simLength :: Floating a => Unit 'NonMetric DLength a
simLength = au

-- | Unit of time used for the simulation
simTime :: Floating a => Unit 'NonMetric DTime a
simTime = day

-- | Unit of mass used for the simulation
simMass :: Floating a => Unit 'NonMetric DMass a
simMass = solarMass

-- | Unit of velocity used for the simulation
simVelocity :: Floating a => Unit 'NonMetric DVelocity a
simVelocity = simLength / simTime

-- | Unit of energy used for the simulation
simEnergy :: Floating a => Unit 'NonMetric DEnergy a
simEnergy = simMass * simLength * simLength / (simTime * simTime)

-- | Newton's Gravitational Constant.
gravitationalConstant :: Floating a => Quantity (DVolume / (DMass * DTime * DTime)) a
gravitationalConstant = 6.67408e-11 *~ ((cubic meter) / (kilo gram * second * second))

-- | Gravitational Constant, expressed in simulation units
simGravitationalConstant :: Floating a => a
simGravitationalConstant = gravitationalConstant ^. (inUnit ((cubic simLength) / (simMass * simTime * simTime)))
