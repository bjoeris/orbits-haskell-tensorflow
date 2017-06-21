{-# LANGUAGE TemplateHaskell #-}

module Orbits.Simulation
  ( Simulation(Simulation)
  , getBodies
  , getEnergy
  , doStep
  ) where

import Control.Lens
import Data.Vector (Vector)
import Numeric.Units.Dimensional (Time)
import Numeric.Units.Dimensional.Quantities (Energy)


import Orbits.System (Body)

-- | A simulation of several bodies interacting gravitationally
data Simulation m a = Simulation
  -- | Get the current bodies
  { _getBodies :: m (Vector (Body a))
  -- | Compute the energy of the system.
  --   This is a useful proxy for simulation stability because the energy should
  --   not change.
  , _getEnergy :: m (Energy a)
  -- | Execute one simulation step, advancing time by the specified amount.
  , _doStep :: Time a -> m ()
  }

makeLenses ''Simulation

