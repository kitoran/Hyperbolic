{-# Language NoMonomorphismRestriction, OverloadedStrings, MultiParamTypeClasses, 
         DeriveFunctor, DeriveGeneric, DataKinds #-}
module Projective where

import GHC.TypeLits

newtype Bas m n a = Array a



project::Bas 1 (S n) -> Bas n (S n) -> Bas 1 (S n) -> Bas 1 n
project