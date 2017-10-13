{-# LANGUAGE GADTs #-}
module Editing where

data Action

data Model

data P a where
    getModel :: P Model
    transform :: Action -> P ()


-- в Editor.hs типа функция run :: PromptT P IO
runrun :: IO ()
runrun = do 
    model <- loadOrCreateModel
    MonadPrompt.runPrompt (\case
        Pure -> model
        smthElse -> blah) run

run :: PromptT P IO
run = do 
    let ?someFrpProbably = False
    action <- somehowWeGetWhatActionUserWantsToDo
    prompt (transform action)
    lift showNewModelToUser =<< prompt getModel
    run

data ViewOptions
data EditingMode
