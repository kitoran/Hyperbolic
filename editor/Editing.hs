{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
-- # LANGUAGE PatternSynonyms  #
module Editing where

import System.IO
import System.Exit
-- import Data.Sequence
import Data.IORef
import Data.Char
import Data.Proxy
import Data.Singletons.TH
import GHC.Exts
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar 
import Debug.Trace
import Control.Monad
-- import qualified Linear as L
data Coordinates = C Int

$(singletons [d|
  data Menu = R | L
  |])
$(singletons [d|
  data EditorState = AddingVertices | AddingEdges | MenuOpened Menu
  |])

data Doing (a::EditorState) (b::EditorState) where -- разбить на изменение view, изменение model и действия типа выхода ( они могут быть в одном типе с изменениями view)  
    AddVertex :: Coordinates -> Doing AddingVertices AddingVertices
    RemoveVertex :: Coordinates -> Doing AddingEdges AddingEdges
    ToggleModeV :: Doing AddingEdges AddingVertices
    ToggleModeE :: Doing AddingVertices AddingEdges
    ClickMenu :: forall a (m::Menu) . Doing a (MenuOpened m) -- dependent much? singletons bla bla type inference
    Quit :: Doing a Any
toggle AddingVertices = AddingEdges
toggle  AddingEdges =AddingVertices
toggle _ = error "this should be a type error"
-- class Brack (e::EditorState) where
    -- brack :: Proxy e -> Model -> String
-- data Singl (e::EditorState) where
   -- Av :: Singl AddingVertices
   -- Ae :: Singl AddingEdges
   -- Mo :: forall (m::Menu) . SMenu m -> Singl (MenuOpened (m))
-- reify :: Singl e -> EditorState
-- reify Av = AddingVertices
-- reify Ae = AddingEdges
-- reify (Mo m) = MenuOpened $ reifym m

-- instance Brack AddingVertices where
    -- brack _ m =  ("\"" ++ show m ++ "\"")
-- instance Brack AddingEdges where
    -- brack _ m =  ("[" ++ show m ++ "]")
-- instance Brack Any where
    -- brack _ m = error "this is precisely the code i don't want to write"-- ("\"" ++ show m ++ "\"")
data State where 
    S :: forall e. SEditorState e -> Model -> State  
-- data SomeState where
    -- SS :: forall e. State -> SomeState
app :: Doing a b -> Doing b c -> Doing a c
app = app

type Number = Double 
data WRER (a::EditorState)
-- data Action = A 
-- type family Apply a b where
    -- Apply (Doing a b) (WRER a) = WRER b
-- apply :: forall a b. Doing a b -> State -> State 
-- apply (AddVertex (C e)) (S _ (M d)) = S (Proxy @b) $ M (e+d)
apply2 :: forall a b. Doing a b -> WRER a -> WRER b
apply2 = apply2
-- apply3 :: forall a b. a -> b -> (Apply a b)
-- apply3 = apply3
-- data Action

newtype Model = M {unM::Int} deriving ( Show, Num)

-- data ViewOptions = VO (L.M44 Double) deriving (Show, Read)

data Event = Keyboard Char | Show deriving ( Eq)

data EditingMode = EM deriving (Enum, Eq)

-- data State = S ViewOptions EditingMode Model 
function :: Doing a b -> Proxy a -> Proxy b
function = function
type family Fam d f where
    Fam (Doing a b) a = b
-- data SpecialEvent = Quit | ToggleView | ToggleMode
-- data Exists = forall e  t . E (Doing e t) 
-- interpret :: forall (r::EditorState) t . Char -> Maybe (Doing r t) 
-- interpret c = Just (AddVertex (C $ read [c]))
pu :: forall k. SEditorState k
pu = pu
processKeyboard :: IORef (State ) -> Char -> IO Bool
processKeyboard stateRef c = do
     atomicModifyIORef stateRef (\s@(S v m ) -> case interpret v c of
        (Just (SD _ (AddVertex (C ff)))) -> (S SAddingVertices (M $ unM m+ff), True)
        (Just (SD _ (RemoveVertex (C ff)))) -> (S SAddingEdges (M $ unM m-ff), True)
        (Just (SD _ Quit)) -> (S pu m, False)
        -- (Just (Left ToggleView)) -> (S (toggle v) e m, True)
        (Just (SD _ ToggleModeE)) -> (S (SAddingEdges) m , True)
        (Just (SD _ ToggleModeV)) -> (S (SAddingVertices) m , True)
        (Nothing) -> (S v m , True))

brack :: SEditorState e -> Model -> String
brack SAddingVertices m =  ("\"" ++ show m ++ "\"")
brack SAddingEdges m =  ("[" ++ show m ++ "]")
brack _ _ =  [] -- ("[" ++ show m ++ "]")
processEvent stateRef event = do
    (S v m ) <- readIORef stateRef
    case event of
     Show -> putStrLn (brack v m) >> return True
            -- AddingEdges -> putStrLn ("(" ++ show m ++ ")") >> return True
     Keyboard c -> processKeyboard stateRef c

data SomeDoing r = forall t . SD (SEditorState t) (Doing r t)
interpret :: SEditorState e -> Char -> Maybe (SomeDoing e)
interpret SAddingVertices c 
  | isDigit c  = Just $ SD SAddingVertices $ AddVertex (C $ read [c]) 
  | (c == 'q') = Just $ SD undefined $ Quit
  | (c == 's') = Just $ SD SAddingEdges $ ToggleModeE
  | otherwise = Nothing
interpret SAddingEdges c 
  | isDigit c = Just $ SD SAddingEdges $ RemoveVertex (C $ read [c]) 
  | (c == 'q') = Just $ SD undefined $ Quit
  | (c == 's') = Just $ SD SAddingVertices $ ToggleModeV -- последние две строки уместить в одну?
  | otherwise = Nothing

  -- | (c == 'd') = Just $ Right $ Double
  -- | (c == 'v') = Just $ Left ToggleView

eventLoop :: IORef (State ) -> Chan Event -> IO ()
eventLoop stateRef queueRef = do
    event <- readChan queueRef
    cont <- processEvent stateRef event
    when cont $ eventLoop stateRef queueRef
    

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    i <- myThreadId
    y <- newIORef (S (SAddingVertices) 4)
    var <- newChan
    sho <- forkIO $ showM var
    rea <- forkIO $ readM var
    eventLoop y var
    killThread sho
    killThread rea
  where
    -- go :: MVar Action -> IORef Model -> IO ()
    -- go y i = do 
    --   d <- takeMVar y
    --   modifyIORef i (apply d)
    --   go y i


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
