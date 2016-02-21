{-# Language NoMonomorphismRestriction, OverloadedStrings, MultiParamTypeClasses, 
         DeriveFunctor, DeriveGeneric, DataKinds #-}
module Projective where

{-

модуль для работы с проекциями. Сейчас мне нужны две операции: спроецировать проективное 
пространство из точки на плоскость и спроецировать плоскость на обычнуювекторную плоскость

-}

import GHC.TypeLits


newtype Bas m n a = Array a



project::Bas 1 (S n) -> Bas n (S n) -> Bas 1 (S n) -> Bas 1 n
project