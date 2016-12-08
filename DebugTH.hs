-- {-# LANGUAGE TemplateHaskell #-}
module DebugTH  where
import Control.Monad
import Language.Haskell.TH
 
prettyR :: String -> Q Exp
prettyR matrixName = do
   -- reactimate $ fmap (\x -> putStrLn $ "\nmoveDelta:\n" ++
   --                                           pretty x)  moveDelta 
  justmatrix <- lookupValueName matrixName
  when (justmatrix == Nothing) (fail "justmatrix == Nothing")
  let Just matrix = justmatrix
  Just putStrLn <- lookupValueName "putStrLn"
  Just pretty <- lookupValueName "pretty"
  Just fmap <- lookupValueName "fmap"
  Just append <- lookupValueName "++"
  Just reactimate <- lookupValueName "reactimate"
  x  <- newName "x"
  return $ AppE (VarE reactimate) (AppE (AppE (VarE fmap)
                                              (LamE [VarP x]
                                                    (AppE (VarE putStrLn)
                                                          (AppE (AppE (VarE append)
                                                                      (LitE (StringL ("\n"++matrixName++ ":\n"))))
                                                                (AppE (VarE pretty)
                                                                      (VarE x))))))
                                         (VarE matrix))


prettyV :: String -> Q Exp
prettyV matrixName = do
   -- reactimate $ fmap (\x -> putStrLn $ "\nmoveDelta:\n" ++
   --                                           pretty x)  moveDelta 
  Just matrix <- lookupValueName matrixName
  Just putStrLn <- lookupValueName "putStrLn"
  Just pretty <- lookupValueName "show"
  Just fmap <- lookupValueName "fmap"
  Just append <- lookupValueName "++"
  Just reactimate <- lookupValueName "reactimate"
  x  <- newName "x"
  return $ AppE (VarE reactimate) (AppE (AppE (VarE fmap)
                                              (LamE [VarP x]
                                                    (AppE (VarE putStrLn)
                                                          (AppE (AppE (VarE append)
                                                                      (LitE (StringL ("\n"++matrixName++ ":\n"))))
                                                                (AppE (VarE pretty)
                                                                      (VarE x))))))
                                         (VarE matrix))

  
