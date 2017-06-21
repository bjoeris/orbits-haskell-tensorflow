{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module TensorFlow.Operators
  ( (^+)
  , (^-)
  , (^*)
  , (^/)
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16)
import Data.Complex (Complex)
import qualified TensorFlow.Core as TF (Tensor, Build, OneOf)
import qualified TensorFlow.GenOps.Core as TF (add, sub, mul, realDiv)

infixl 6 ^+, ^-
infixl 7 ^*, ^/

(^+) :: TF.OneOf '[Complex Double, Complex Float, Int16, Int32, Int64, Int8, Word16, Word8, Double, Float] t
     => TF.Tensor v'1 t
     -> TF.Tensor v'2 t
     -> TF.Tensor TF.Build t
a ^+ b = a `TF.add` b

(^-) :: TF.OneOf '[Complex Double, Complex Float, Int32, Int64, Word16, Double, Float] t
     => TF.Tensor v'1 t
     -> TF.Tensor v'2 t
     -> TF.Tensor TF.Build t
a ^- b = a `TF.sub` b

(^*) :: TF.OneOf '[Complex Double, Complex Float, Int16, Int32, Int64, Int8, Word16, Word8, Double, Float] t
     => TF.Tensor v'1 t
     -> TF.Tensor v'2 t
     -> TF.Tensor TF.Build t
a ^* b = a `TF.mul` b

(^/) :: TF.OneOf '[Complex Double, Complex Float, Int16, Int32, Int64, Int8, Word16, Word8, Double, Float] t
     => TF.Tensor v'1 t
     -> TF.Tensor v'2 t
     -> TF.Tensor TF.Build t
a ^/ b = a `TF.realDiv` b
