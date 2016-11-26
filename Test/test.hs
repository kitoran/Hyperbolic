import Test.QuickCheck

import Debug.Trace
import Linear hiding (distance, trace)

import Hyperbolic
import Control.Lens

instance (Arbitrary a, Num a, Ord a) => Arbitrary (Point a) where
    arbitrary = fmap (\[a, b, c,d] -> head $ dropWhile (not.proper) $ iterate (& (_v4._w) %~ (+1)) $ Point a b c d) $ vectorOf 4 arbitrary 

(=@=) :: M44 Double -> M44 Double -> Bool
a =@= b = all (=!= (V4 0 0 0 0)) $ a ^-^ b 
infix 4 =@=

--(=!=) :: (Fractional b, Ord b, Foldable t, Additive t) => t b -> t b -> Bool
a =!= b = all (=.= 0) $ a ^-^ b 
infix 4 =!=

(=.=) :: Double -> Double -> Bool
a =.= b = abs (a-b) < 0.01

testMoveTo :: (Point Double -> Double -> M44 Double) -> Point Double -> Double -> Bool
testMoveTo f to dist = if proper to
                       then let point = over _v4 (f to dist !*) to in distance origin point =.= abs (distance origin to + dist)
                       else (trace "nonapplicabe" True)



testinverseZ :: (M44 Double -> M44 Double) -> Double -> Bool
testinverseZ f alpha = a !*! (f a) =@= identity
                where a = rotateAroundZ alpha
testinverseY :: (M44 Double -> M44 Double) -> Double -> Bool
testinverseY f alpha = a !*! (f a) =@= identity
                where a = rotateAroundY alpha