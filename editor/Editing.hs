{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
-- # LANGUAGE PatternSynonyms  #
module Editing where

import System.IO
import System.Exit
-- import Data.Sequence
import Data.IORef
import Data.Char
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar 
import Control.Monad
import qualified Linear as L

data Menu = ...
data EditorState = AddingVertices | AddingVertices | MenuOpened Menu
data Doing EditorState EditorState where 
    AddVertex :: Coordinates -> Doing AddingVertices AddingVertices
    ClickMenu :: (m::Menu) -> Doing a (MenuOpened m) -- dependent much? singletons bla bla type inference
do :: Doing a b -> State a -> State b
type Number = Double 

data Action = A 

apply :: Action -> Model -> Model
apply a = case a of A -> id
-- data Action

data Model = M
data ViewOptions = VO (L.M44 Double)deriving (Enum)

-- data Event = Keyboard Char | Show

data EditingMode = EM deriving (Enum, Eq)

data State = S ViewOptions EditingMode Model 

-- data SpecialEvent = Quit | ToggleView | ToggleMode

processKeyboard stateRef c = do
    cont <- atomicModifyIORef stateRef id{-(\(S v e m) -> case interpret e c of
        (Just (Right a)) -> (S v e $ apply a m, True)
        (Just (Left Quit)) -> (S v e m, False)
        (Just (Left ToggleView)) -> (S (toggle v) e m, True)
        (Just (Left ToggleMode)) -> (S v (toggle e) m, True)
        (Nothing) -> (S v e m, True))-}

processEvent stateRef (Show) = do
    (S v _ m) <- readIORef stateRef 
    case v of
        Quotes -> putStrLn $ "\"" ++ show m ++ "\""
        Parens -> putStrLn $ "(" ++ show m ++ ")"
    return True

interpret e c = 
  | isDigit c && e == AddMode = Just $ Right $ Add (read [c]) 
  | isDigit c && e == SubtractMode = Just $ Right $ Subtract (read [c]) 
  | (c == 'd') = Just $ Right $ Double
  | (c == 'q') = Just $ Left Quit
  | (c == 's') = Just $ Left ToggleMode
  | (c == 'v') = Just $ Left ToggleView
  | otherwise = Nothing

eventLoop :: IORef State -> Chan Event -> IO ()
eventLoop stateRef queueRef = do
    event <- readChan queueRef
    cont <- processEvent stateRef event
    when cont $ eventLoop stateRef queueRef
    

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    i <- myThreadId
    y <- newIORef (S Quotes AddMode 5)
    var <- newChan
    sho <- forkIO $ showM var
    rea <- forkIO $ readM var
    eventLoop y var
    killThread sho
    killThread rea
  where
    go :: MVar Action -> IORef Model -> IO ()
    go y i = do 
      d <- takeMVar y
      modifyIORef i (apply d)
      go y i


readM :: Chan Event -> IO ()
readM y = do
      d <- getChar
      putStr "\n"
      writeChan y (Keyboard d) 
      readM y

showM :: Chan Event -> IO ()
showM y = do
  writeChan y Show 
  threadDelay 1000000 
  showM y

-- Единственная проблема - мне так и не удалось использовать GADTs. Не понимаю где все находят эти юзкейсы.

-- https://mail.haskell.org/pipermail/haskell-cafe/2008-January/038151.html
