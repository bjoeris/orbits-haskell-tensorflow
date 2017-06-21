{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Orbits.System
  ( System
  , NamedBody
  , Body(Body)
  , position
  , velocity
  , mass
  , name
  , body
  , bodies
  , names
  , time
  , bodiesFromVecs
  ) where

import Data.Aeson ((.:), FromJSON, Value(Object), parseJSON)
import Data.Text (Text)
import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import Control.Lens ((^.), (%~), (&), makeLenses, mapping)
import Linear.V3 (V3(V3), _x, _y, _z)
import qualified Numeric.Units.Dimensional as D
import Numeric.Units.Dimensional ((*~), (/~), Unit, Quantity, Length, Mass, Time)
import Numeric.Units.Dimensional.Quantities (Velocity)

import Orbits.Units

-- | A body in the system
data Body a = Body
  { _position :: V3 (Length a)
  , _velocity :: V3 (Velocity a)
  , _mass :: Mass a
  }

-- | A body with a name.
--   Used to simplify parsing the YAML system definition
data NamedBody a = NamedBody
  { _name :: Text
  , _body :: Body a
  }

-- | A system of many bodies, interacting gravitationally
data System a = System
  { _bodies :: Vector (Body a) -- ^ the bodies in the system
  , _names :: Vector Text      -- ^ the names of the bodies in the system,
                               --   in the same order as _bodies
  , _time :: Time a            -- ^ current time of the system,
                               --   from some arbitrary start time
  }

makeLenses ''Body
makeLenses ''NamedBody
makeLenses ''System

-- | Helper for constructing a system from 'NamedBody's
system :: Vector (NamedBody a) -> Time a -> System a
system namedBodies = System bodies names
  where
    (names, bodies) = V.unzip $ (\named -> (named^.name, named^.body)) <$> namedBodies

v3Quant :: Num a => Unit m d a -> a -> a -> a -> V3 (Quantity d a)
v3Quant u x y z = V3 x y z & traverse %~ (D.*~ u)

instance (Floating a, FromJSON a) => FromJSON (Body a) where
  parseJSON (Object v) =
    Body <$>
    (v3Quant au <$> v .: "x" <*> v .: "y" <*> v .: "z") <*>
    (v3Quant (au D./ day) <$> v .: "vx" <*> v .: "vy" <*> v .: "vz") <*>
    ((*~ solarMass) <$> (v .: "mass"))

instance (Floating a, FromJSON a) => FromJSON (NamedBody a) where
  parseJSON (Object v) =
    NamedBody <$>
    v .: "name" <*>
    parseJSON (Object v)

instance (Floating a, FromJSON a) => FromJSON (System a) where
  parseJSON (Object v) =
    system <$>
    v .: "bodies" <*>
    ((*~ day) <$> (v .: "time"))

bodiesFromVecs :: Floating a => (Vector a, Vector a, Vector a) -> Vector (Body a)
bodiesFromVecs (positions, velocities, masses) = V.imap mkBody masses
  where
    mkV3 unit vec i = V3 (vec ! (i*3)) (vec ! (i*3+1)) (vec ! (i*3+2)) ^. mapping (fromUnit unit)
    mkBody i m = Body
      (mkV3 simLength positions i)
      (mkV3 simVelocity velocities i)
      ((masses ! i) D.*~ simMass)
