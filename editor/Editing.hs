module Editing where

data Action

data Model

data P a where
    getModel :: P Model
    tramsform :: Action -> P ()