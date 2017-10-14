{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Editing where

import System.IO
import System.Exit
import Data.IORef
import Data.Char
import Control.Concurrent
import Control.Concurrent.MVar 
import Control.Monad

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

newtype Model = M Int deriving (Num, Show)
data Action = Double | Add Int

main :: IO ()
main = do
    i <- myThreadId
    y <- newIORef 5
    var <- newEmptyMVar
    sho <- forkIO $ showM y
    rea <- forkIO $ readM [i, sho] var
    hSetBuffering stdin NoBuffering
    go var y
    killThread sho
    killThread rea
  where
    go :: MVar Action -> IORef Model -> IO ()
    go y i = do 
      d <- takeMVar y
      modifyIORef i (apply d)
      go y i

apply :: Action -> Model -> Model
apply Double = (*2)
apply (Add e) = (+M e)

readM :: [ThreadId] -> MVar Action -> IO ()
readM th y = do
      d <- getChar
      putStrLn ""
      when (isDigit d)
          (putMVar y (Add (read [d])) )
      when (d == 'd')
          (putMVar y Double )
      when (d == 'q') $ mapM_ killThread th 
      when (d /= 'q') $ readM th y

showM :: IORef Model -> IO ()
showM y = do
  (print =<< readIORef y) 
  threadDelay 1000000 
  showM y

-- мне нравится идея, что вычисления происходят не в гуишном треде, но по мне более красиво было бы, если бы не было forkIO, а функция изменения состояния была
-- колбэком у readM (в реальной программе ввод и вывод в одном треде, это просто с консолью такие трудности).
-- Единственная проблема - мне так и не удалось использовать GADTs. Не понимаю где все находят эти юзкейсы.