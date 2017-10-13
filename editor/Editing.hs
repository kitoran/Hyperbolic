{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Editing where

import System.IO
import Control.Concurrent

-- data Action

-- data Model

-- data P a where
--     GetModel :: P Model
--     Transform :: Action -> P ()


-- -- в Editor.hs типа функция run :: PromptT P IO
-- runrun :: IO ()
-- runrun = do 
--     model <- loadOrCreateModel
--     MonadPrompt.runPrompt (\case
--         Pure -> model
--         smthElse -> blah) run

-- run :: PromptT P IO
-- run = do 
--     let ?someFrpProbably = ?keeraHails
--     action <- somehowWeGetWhatActionUserWantsToDo
--     prompt (Transform action)
--     lift showNewModelToUser =<< prompt GetModel
--     run

-- data ViewOptions
-- data EditingMode

-- let's say Model = Int
-- the problem is we can't block, вычисление инициируется GUI
-- может, можно использовать MVar и вообще
newtype Model = M Int
data Action = Double | Add Int
main :: IO ()
main = do
    forkIO ()
    hSetBuffering stdin NoBuffering 
    d <- getChar 
    print d