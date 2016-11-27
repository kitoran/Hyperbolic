{-# Language NoMonomorphismRestriction, OverloadedStrings,
             MultiParamTypeClasses, DeriveFunctor, DeriveGeneric, ScopedTypeVariables #-}

import Test.QuickCheck
import Test.QuickCheck.Property

import Debug.Trace
import Linear hiding (distance, trace, normalize)

import Hyperbolic
import Control.Lens

instance (Arbitrary a,  Ord a, Fractional a) => Arbitrary (Point a) where
    arbitrary = fmap (\[a, b, c,d] ->  head $ dropWhile (not.proper) $ iterate (& (_v4._w) %~ (+1)) $ Point a b c d) $ vectorOf 4 arbitrary 

(=@=) :: M44 Double -> M44 Double -> Bool
a =@= b = all (=!= (V4 0 0 0 0)) $ a ^-^ b 
infix 4 =@=

--(=!=) :: (Fractional b, Ord b, Foldable t, Additive t) => t b -> t b -> Bool
a =!= b = all (=.= 0) $ a ^-^ b 
infix 4 =!=

(Point a b c d) =!!= (Point e f g h) = a =.= e && b =.= f && c =.= g && d =.= h 
infix 4 =!!=


(=...=) :: Double -> Double -> String -> Property
a =...= b = \s -> whenFail' (putStr (show a) >> putStr " /= " >> putStr (show b) >> putStrLn s) (abs (a-b) < 0.01)
(=..=) :: Double -> Double -> Property
a =..= b = whenFail' (putStr (show a) >> putStr " /= " >> putStr (show b) >> putStrLn "") (abs (a-b) < 0.01)
(=.=) :: Double -> Double -> Bool
a =.= b = (abs (a-b) < 0.01)

(/.=) a = not . (=.= a) 

testMoveTo :: Point Double -> Double -> Property
testMoveTo to dist = (proper to && abs dist < 13.125) ==> (let point = over _v4 (moveTo to dist !*) to 
                                                               formDist = distance origin to 
                                                               newDist = distance origin point 
                                                               info = show dist ++ " " ++ show formDist ++ " " ++ show newDist in 
         {-}      if(formDist < dist) then (( newDist =...= abs (dist - formDist) $ " (diff)" ++ info) )
                                                           else (({ -} newDist =..= abs(dist + formDist) {-$ " (absum)" ++ info ))-})
dfo = distance origin

testMoveSimpl to = (proper to && dfo to   < 13.125) ==> (let point = over _v4 (moveTo to (-formDist) !*) to 
                                                             formDist = distance origin to 
                                                             newDist = distance origin point in 
              ( newDist =..= 0 ))


testMoveTo1 :: Point Double  -> Property
testMoveTo1   to      = if proper to
                        then let point = over _v4 (moveTo to 1 !*) to in distance origin point =..= abs (distance origin to + 1) .||.  distance origin point =..= abs (distance origin to - 1) 
                        else (error  "nonapplicabe" True)


testinverseZ :: (M44 Double -> M44 Double) -> Double -> Bool
testinverseZ f alpha = a !*! (f a) =@= identity
                where a = rotateAroundZ alpha
testinverseY :: (M44 Double -> M44 Double) -> Double -> Bool
testinverseY f alpha = a !*! (f a) =@= identity
                where a = rotateAroundY alpha

testInvariant1 f p d = f p =!= f (fmap (*d) p)

testInvariant2 f p p1 d d1 = d /.= 0 && d1 /.= 0 ==> f p p1 =.= f (fmap (*d) p) (fmap (*d1) p1)

triangleInequality p r s = 
       distance p r + distance r s >= distance p s

symmetry p r = distance p r =.= distance r p

onPositiveX px = ((signum $ px ^. _v4 . _x) == 0 .||. (signum $ px ^. _v4 . _x) == (signum $  px ^. _v4 . _t))

testTurmPToOxz p = (turmPToOxz p !$ p ^. _v4 . _y) =..= 0 .&&.  (onPositiveX $ turmPToOxz p !$ p)

testTurmPToOx p = let px = turmPToOx p !$ p in (px ^. _v4 . _y) =..= 0 .&&. 
                                               (px  ^. _v4 . _z) =..= 0 .&&. 
                                               onPositiveX px .&&. 
                                               distance origin p =..= distance origin px 

testTurmPToOxb p = (turmPToOx p !$ p ^. _v4 . _y) =.= 0 && (turmPToOx p !$ p ^. _v4 . _z) =.= 0

isometry m p r = distance p r =.= distance (m !$ p) (m !$ r)
main = verboseCheck (isometry . moveAlongX )

check = verboseCheckWith (stdArgs {maxDiscardRatio = 100000})