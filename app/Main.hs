{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad (forM_, when, foldM, join, (>>))
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int32, Int64)
import Data.List (genericLength)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Control.Lens hiding ((*~), (/~), au)
import Data.Vector.Lens (toVectorOf)
import Foreign.Storable (Storable, sizeOf, alignment, poke, pokeElemOff, peek, peekElemOff)
import Foreign.Ptr (castPtr)
import System.IO (IOMode(WriteMode), withFile)

import qualified TensorFlow.Core as TF
import qualified TensorFlow.Ops as TF hiding (initializedVariable, zeroInitializedVariable)
import qualified TensorFlow.GenOps.Core as TF (realDiv, square, sqrt, slice, matrixSetDiag, squeeze, print, print', tile)
import qualified TensorFlow.Variable as TF
import qualified TensorFlow.Minimize as TF
import qualified TensorFlow.Logging as TF
import qualified TensorFlow.Types as TF (ListOf((:/),Nil))

import Data.ProtoLens.Encoding (decodeMessageOrDie, encodeMessage)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Array.Repa (Array, U, DIM1, DIM2, Z(Z), (:.)((:.)))
import qualified Data.Array.Repa as A
import Data.Array.Repa.Repr.Vector (V)
import Linear.V3
import qualified Numeric.Units.Dimensional as D
import Numeric.Units.Dimensional ((*~), (/~), Length, Time, Mass, Unit, Metricality(Metric,NonMetric), DLength, DMass, DTime, mkUnitR)
import Numeric.Units.Dimensional.Quantities (Velocity, Energy, Volume, DVelocity, DEnergy, DVolume, cubic)
import Numeric.Units.Dimensional.SIUnits (meter, gram, day, kilo, second)
import Numeric.Units.Dimensional.NonSI (year)
import Numeric.Units.Dimensional.UnitNames (atom)

import Data.Yaml (FromJSON(..), (.:))
import qualified Data.Yaml as Y
import qualified Data.Csv as Csv

import System.Exit (exitFailure)

v3FromUnit :: Floating a => Unit m d a -> V3 a -> V3 (D.Quantity d a)
v3FromUnit unit (V3 x y z) =
  V3 (x *~ unit)
     (y *~ unit)
     (z *~ unit)

v3InUnit :: Floating a => Unit m d a -> V3 (D.Quantity d a) -> V3 a
v3InUnit unit (V3 x y z) =
  V3 (x /~ unit)
     (y /~ unit)
     (z /~ unit)

data Body a = Body
  { _name :: Text
  , _position :: V3 (Length a)
  , _velocity :: V3 (Velocity a)
  , _mass :: Mass a
  }

data System a = System
  { _bodies :: Vector (Body a)
  , _time :: Time a
  }

makeLenses ''Body
makeLenses ''System

instance (Floating a, FromJSON a) => FromJSON (Body a) where
  parseJSON (Y.Object v) =
    Body <$>
    v .: "name" <*>
    (v3FromUnit au <$> (V3 <$> v .: "x" <*> v .: "y" <*> v .: "z")) <*>
    (v3FromUnit (au D./ day) <$> (V3 <$> v .: "vx" <*> v .: "vy" <*> v .: "vz")) <*>
    ((*~ solarMass) <$> (v .: "mass"))

instance (Floating a, FromJSON a) => FromJSON (System a) where
  parseJSON (Y.Object v) =
    System <$>
    v .: "bodies" <*>
    ((*~ day) <$> (v .: "time"))

bodyPosX :: Int
bodyPosX = 0
bodyPosY :: Int
bodyPosY = 1
bodyPosZ :: Int
bodyPosZ = 2
bodyVelX :: Int
bodyVelX = 3
bodyVelY :: Int
bodyVelY = 4
bodyVelZ :: Int
bodyVelZ = 5
bodyMass :: Int
bodyMass = 6
bodyNumFields :: Int
bodyNumFields = 7

bodyToVec :: (Floating a) => Body a -> Vector a
bodyToVec body =
  [ body^.position._x /~ simLength
  , body^.position._y /~ simLength
  , body^.position._z /~ simLength
  , body^.velocity._x /~ simVelocity
  , body^.velocity._y /~ simVelocity
  , body^.velocity._z /~ simVelocity
  , body^.mass        /~ simMass
  ]

bodyFromVec :: (Floating a) => Text -> Vector a -> Body a
bodyFromVec name vec
  | V.length vec == 7 = Body
    { _name = name
    , _position = V3 (vec ! bodyPosX *~ simLength)
                     (vec ! bodyPosY *~ simLength)
                     (vec ! bodyPosZ *~ simLength)
    , _velocity = V3 (vec ! bodyVelX *~ simVelocity)
                     (vec ! bodyVelY *~ simVelocity)
                     (vec ! bodyVelZ *~ simVelocity)
    , _mass = (vec ! bodyMass) *~ simMass
    }
  | otherwise = error $
                "Unable to extract Body from array of length "
                ++ show (V.length vec)

bodiesToTensor :: Vector (Body Double) -> TF.TensorData Double
bodiesToTensor bodies = TF.encodeTensorData
                        [fromIntegral $ V.length bodies, fromIntegral bodyNumFields]
                        (bodies >>= bodyToVec)

bodiesFromVec :: (Floating a) => Vector Text -> Vector a -> Vector (Body a)
bodiesFromVec names vec = V.generate numBodies getBody
  where
    numBodies = V.length names
    getBody i = bodyFromVec (names ! i) $ V.slice (i * bodyNumFields) bodyNumFields vec


-- UNITS
au :: Floating a => Unit 'NonMetric DLength a
au = mkUnitR (atom "au" "au" "astronomical unit") 1.49597870700e+11 meter

solarMass :: Floating a => Unit 'NonMetric DMass a
solarMass = mkUnitR (atom "mS" "mS" "solar mass") 1.98855e+30 (kilo gram)

simLength :: Floating a => Unit 'NonMetric DLength a
simLength = au

simTime :: Floating a => Unit 'NonMetric DTime a
simTime = year

simMass :: Floating a => Unit 'NonMetric DMass a
simMass = solarMass

simVelocity :: Floating a => Unit 'NonMetric DVelocity a
simVelocity = simLength D./ simTime

simEnergy :: Floating a => Unit 'NonMetric DEnergy a
simEnergy = simMass D.* simLength D.* simLength D./ (simTime D.* simTime)

gravitationalConstant :: Floating a => D.Quantity (DVolume D./ (DMass D.* DTime D.* DTime)) a
gravitationalConstant = 6.67408e-11 *~ ((cubic meter) D./ (kilo gram D.* second D.* second))

-- SOLAR SYSTEM BODIES
sol :: Floating a => Body a
sol = Body
  { _name = "Sol"
  , _position = V3 (0 *~ au) (0 *~ au) (0 *~ au)
  , _velocity = V3 (0 *~ (au D./ day)) (0 *~ (au D./ day)) (0 *~ (au D./ day))
  , _mass = 1 *~ solarMass
  }

jupiter :: Floating a => Body a
jupiter = Body
  { _name = "Jupiter"
  , _position = V3 (4.84143144246472090e+00 *~ au)
                ((-1.16032004402742839e+00) *~ au)
                ((-1.03622044471123109e-01) *~ au)
  , _velocity = V3 (1.66007664274403694e-03 *~ (au D./ day))
                (7.69901118419740425e-03 *~ (au D./ day))
                ((-6.90460016972063023e-05) *~ (au D./ day))
  , _mass = 9.54791938424326609e-04 *~ solarMass
  }

saturn :: Floating a => Body a
saturn = Body
  { _name = "Saturn"
  , _position = V3 (8.34336671824457987e+00 *~ au)
                (4.12479856412430479e+00 *~ au)
                ((-4.03523417114321381e-01) *~ au)
  , _velocity = V3 ((-2.76742510726862411e-03) *~ (au D./ day))
                (4.99852801234917238e-03 *~ (au D./ day))
                (2.30417297573763929e-05 *~ (au D./ day))
  , _mass = 2.85885980666130812e-04 *~ solarMass
  }

uranus :: Floating a => Body a
uranus = Body
  { _name = "Uranus"
  , _position = V3 (1.28943695621391310e+01 *~ au)
                ((-1.51111514016986312e+01) *~ au)
                ((-2.23307578892655734e-01) *~ au)
  , _velocity = V3 (2.96460137564761618e-03 *~ (au D./ day))
                (2.37847173959480950e-03 *~ (au D./ day))
                ((-2.96589568540237556e-05) *~ (au D./ day))
  , _mass = 4.36624404335156298e-05 *~ solarMass
  }

neptune :: Floating a => Body a
neptune = Body
  { _name = "Neptune"
  , _position = V3 (1.53796971148509165e+01 *~ au)
                ((-2.59193146099879641e+01) *~ au)
                (1.79258772950371181e-01 *~ au)
  , _velocity = V3 (2.68067772490389322e-03 *~ (au D./ day))
                (1.62824170038242295e-03 *~ (au D./ day))
                ((-9.51592254519715870e-05) *~ (au D./ day))
  , _mass = 5.15138902046611451e-05 *~ solarMass
  }

solar_system :: Floating a => System a
solar_system = System
  { _bodies = [sol, jupiter, saturn, uranus, neptune]
  , _time = 0 *~ year
  }


-- SIMULATION

type SimulationStep a = TF.EventWriter -> Time a -> System a -> Int64 -> TF.Session (System a)
type SimulationEnergy a = System a -> TF.Session (Energy a)

int32Scalar :: Int32 -> TF.Tensor TF.Build Int32
int32Scalar i = TF.scalar i

int32Vector :: [Int32] -> TF.Tensor TF.Build Int32
int32Vector vec = TF.vector vec

createSimulation :: TF.Build (SimulationStep Double, SimulationEnergy Double)
createSimulation = do
  let unknownDimension = -1  -- indicates an unknown number to Tensorflow
  -- positions <-
  --   TF.placeholder [unknownDimension, 3]
  -- velocities <-
  --   TF.placeholder [unknownDimension, 3]
  -- masses <-
  --   TF.placeholder [unknownDimension]
  bodiesInput <- TF.placeholder [unknownDimension, 7]
  timeDelta <- TF.placeholder []

      -- (dx, dy, dz) for each pair
  let log name tensor = return tensor
        -- TF.print' (set (TF.opAttr "summarize") (100 :: Int64) .
        --            set (TF.opAttr "message") (name :: BS.ByteString))
        -- tensor
        -- (tensor TF.:/ TF.Nil)
  bodiesInput' <- log "bodiesInput'" $ TF.transpose bodiesInput (int32Vector [1, 0])
  positions <- log "positions" $ TF.slice bodiesInput' (int32Vector [0, 0]) (int32Vector [3, -1])
  velocities <- log "velocities" $ TF.slice bodiesInput' (int32Vector [3, 0]) (int32Vector [3, -1])
  masses <- log "masses" $ TF.slice bodiesInput' (int32Vector [6, 0]) (int32Vector [1, -1])

  positionDiffs <- log "positionDiffs" $ (TF.expandDims positions (int32Scalar 2)) `TF.sub`
                   (TF.expandDims positions (int32Scalar 1))

      -- dx^2 + dy^2 + dz^2 for each pair
  distance2 <- log "distance2" $ TF.sum (TF.square positionDiffs) (int32Scalar 0)
  distance <- log "distance" $ TF.sqrt distance2

      -- unit vector pointing between each pair
  unitDiffs <- log "unitDiffs" $ positionDiffs `TF.realDiv` (TF.expandDims distance (int32Scalar 0))

  g <- log "g" $ TF.scalar (gravitationalConstant /~ ((cubic simLength) D./ (simMass D.* simTime D.* simTime)))

  accelerations' <- log "accelerations'" $ g `TF.mul` unitDiffs `TF.mul`
                       TF.expandDims masses (int32Scalar 2) `TF.realDiv`
                       (TF.expandDims distance2 (int32Scalar 0))

  -- foo <- TF.render $ int32Vector [1]
  -- zeroDiag <- log "zeroDiag" $ TF.tile (TF.constant [3, 1] [0, 0, 0])
  --             (TF.concat (int32Scalar 0) [foo, numBodies])
  accelerations <- log "accelerations" $ TF.matrixSetDiag accelerations' (TF.zerosLike positions)

  totalAcceleration <- log "totalAcceleration" $ TF.sum accelerations (int32Scalar 1)

  newVelocities <- log "newVelocities" $ velocities `TF.add` (timeDelta `TF.mul` totalAcceleration)
  newPositions <- log "newPositions" $ positions `TF.add` (timeDelta `TF.mul` newVelocities)

  bodiesOutput' <- TF.render $ TF.concat (TF.scalar 0)
                   [ newPositions, newVelocities, masses ]
  bodiesOutput' <- log "bodiesOutput'" $ bodiesOutput'
  bodiesOutput <- log "bodiesOutput" $ TF.transpose bodiesOutput' (int32Vector [1, 0])

  massProd <- log "massProd" $ (TF.expandDims masses (int32Scalar 1)) `TF.mul`
                 (TF.expandDims masses (int32Scalar 2))

  kineticEnergies <- log "kineticEnergies" $ masses `TF.mul` TF.sum (TF.square velocities) (int32Scalar 0)
  potentialEnergies <- log "potentialEnergies" $ TF.neg $ g `TF.mul` massProd `TF.realDiv` distance
  energies <- log "energies" $ (TF.scalar 0.5) `TF.mul` TF.matrixSetDiag potentialEnergies kineticEnergies
  energyOutput <- TF.render $ TF.squeeze (TF.sum energies (int32Vector [1, 2]))
  energyOutput <- log "energyOutput" energyOutput

  TF.scalarSummary "energy" energyOutput

  summaries <- TF.mergeAllSummaries

  let feedTimeDelta dt = TF.feed timeDelta $ TF.encodeTensorData [] (V.singleton (dt /~ year))
      feedBodies bodies = TF.feed bodiesInput $ bodiesToTensor bodies
      doStep eventWriter dt system i = do
        (bodiesVec, summaryBytes) <- TF.runWithFeeds
                                     [ feedTimeDelta dt
                                     , feedBodies (system^.bodies) ]
                                     (bodiesOutput, summaries)
        let summary = decodeMessageOrDie (TF.unScalar summaryBytes)
            newBodies = bodiesFromVec (system & toVectorOf (bodies.traverse.name)) bodiesVec
            newTime = system^.time D.+ dt
        TF.logSummary eventWriter i summary
        return $ system & bodies .~ newBodies & time .~ newTime
      getEnergy system = (*~ simEnergy) <$>
                         TF.unScalar <$>
                         TF.runWithFeeds
                         [ feedBodies (system^.bodies) ]
                         energyOutput
  return (doStep, getEnergy)

saveBodyHistory :: (Floating a, Csv.ToField a) => FilePath -> Text -> [System a] -> IO ()
saveBodyHistory fileName bodyName history =
  withFile fileName WriteMode $ \file ->
    forM_ history $ \sys ->
      let t = (sys^.time) /~ simTime
      in forM_ (sys^.bodies) $ \body ->
          when (body^.name == bodyName) $
            let position' = v3InUnit simLength (body^.position)
                velocity' = v3InUnit simVelocity (body^.velocity)
            in LBS.hPut file $ Csv.encode
              [( t,  position'^._x, position'^._y, position'^._z
                , velocity'^._x, velocity'^._y, velocity'^._z)]

main :: IO ()
main = TF.runSession $ TF.withEventWriter "logs" $ \eventWriter -> do
    let graphDef = TF.asGraphDef createSimulation
    liftIO $ BS.writeFile "graphDef.pb" $ encodeMessage graphDef

    (doStep, getEnergy) <- TF.build createSimulation

    system0 <- liftIO $ (either
      (\err -> putStrLn (Y.prettyPrintParseException err) >> exitFailure)
      (\sys -> return sys) =<<
      Y.decodeFileEither "solar_system.yaml")

    let dt = 0.001 *~ year
        printEnergy system energy =
          liftIO $ putStrLn $
          "energy at time " ++ D.showIn simTime (system^.time) ++
          " = " ++ D.showIn simEnergy energy
        step history@(sys : _) i = do
          (: history) <$> doStep eventWriter dt sys i

    energy0 <- getEnergy system0
    printEnergy system0 energy0

    systems <- foldM step [system0] ([0..10000] :: [Int64])

    let system1 = head systems
    energy1 <- getEnergy system1
    printEnergy system1 energy1

    liftIO $ forM_ (system0^.bodies) $ \body -> do
      saveBodyHistory
        (T.unpack (body^.name) ++ ".csv")
        (body^.name)
        systems
