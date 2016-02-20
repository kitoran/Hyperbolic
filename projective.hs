{-# Language NoMonomorphismRestriction, OverloadedStrings, MultiParamTypeClasses, 
         DeriveFunctor, DeriveGeneric, DataKinds #-}
module Projective where

import GHC.TypeLits


project::projective (S n) -> projective (n::Nat)