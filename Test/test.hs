import Test.QuickCheck

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


(=.=) :: Double -> Double -> Bool
a =.= b = abs (a-b) < 0.01

(/.=) a = not . (=.= a) 

testMoveTo :: (Point Double -> Double -> M44 Double) -> Point Double -> Double -> Bool
testMoveTo f to dist = if proper to
                       then let point = over _v4 (f to dist !*) to in distance origin point =.= abs (distance origin to + dist) ||  distance origin point =.= abs (distance origin to - dist) 
                       else (trace "nonapplicabe" True)


testMoveTo1 :: (Point Double -> Double -> M44 Double) -> Point Double  -> Bool
testMoveTo1 f to      = if proper to
                        then let point = over _v4 (f to 1 !*) to in distance origin point =.= abs (distance origin to + 1) ||  distance origin point =.= abs (distance origin to - 1) 
                        else (trace "nonapplicabe" True)


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

isometry m p r = distance p r =.= distance (m !$ p) (m !$ r)
main = verboseCheck (isometry . moveAlongX )

