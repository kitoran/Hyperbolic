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

(Point a b c d) =!!= (Point e f g h) = (not (isNaN a)) && 
                                       (not (isNaN b)) &&
                                       (not (isNaN c)) &&
                                       (not (isNaN d)) &&
                                       (not (isNaN e)) &&
                                       (not (isNaN f)) &&
                                       (not (isNaN g)) &&
                                       (not (isNaN h)) && (a/d) =.= (e/h) && (b/d) =.= (f/h) && (c/d) =.= (g/h)  
infix 4 =!!=
(Point a b c d) =!!!= (Point e f g h) = (not (isNaN a)) .&&. 
                                        (not (isNaN b)) .&&.
                                        (not (isNaN c)) .&&.
                                        (not (isNaN d)) .&&.
                                        (not (isNaN e)) .&&.
                                        (not (isNaN f)) .&&.
                                        (not (isNaN g)) .&&.
                                        (not (isNaN h)) .&&. (a/d) =..= (e/h) .&&. (b/d) =..= (f/h) .&&. (c/d) =..= (g/h)
infix 4 =!!!=


(=...=) :: Double -> Double -> String -> Property
a =...= b = \s -> whenFail' (putStr (show a) >> putStr " /= " >> putStr (show b) >> putStrLn s) ((not (isNaN a)) .&&. 
                                                                                                 (not (isNaN b)) .&&.
                                                                                                 abs (a-b) < 0.01)
(=..=) :: Double -> Double -> Property
a =..= b = whenFail' (putStr (show a) >> putStr " /= " >> putStr (show b) >> putStrLn "") ((not (isNaN a)) .&&. 
                                                                                           (not (isNaN b)) .&&.
                                                                                           abs (a-b) < 0.01)
(=.=) :: Double -> Double -> Bool
a =.= b = ((not (isNaN a)) && 
           (not (isNaN b)) &&
           abs (a-b) < 0.01)

x /.= a = not (x =.= a) && (not (isNaN x))

-- proportional a b c d = (c <= 0.001 && d <= 0.001 && a <= 0.01 && b <= 0.01) ||
--                        (c <= 0.001 && a <= 0.01 ) ||
--                        (c >=

coll :: Point Double -> Point Double  -> Point Double  -> Property
coll a b c = (abs r >= 0.001 && abs s >= 0.001 && abs t >= 0.001) ==> (x/r) =..= (y/s) .&&. (y/s) =..= (z/t)
  where V3 x y z = normalizePoint (toV4 b ^-^ toV4 a)
        V3 r s t = normalizePoint (toV4 c ^-^ toV4 a)

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

testMoveFromTo :: Point Double -> Point Double -> Double -> Property
testMoveFromTo fr to d = (proper to && proper fr && abs d < 10 && distance origin fr < 10 && distance origin to < 10 && distance fr to < 10) ==> let m = moveFromTo fr to d in distance fr (m !$ fr) =..= abs d .&&.  distance to (m !$ to) =..= abs d 


testMoveFromTo1 :: Point Double -> Point Double -> Double -> Property
testMoveFromTo1 fr to d = (proper to && (distance fr to) < 13.125) ==> let m = moveFromTo fr to d in coll (m !$ fr) to fr .&&.  coll (m !$ to) to fr

testMoveFromTo2 :: Point Double -> Point Double -> Property
testMoveFromTo2 fr to = (proper to && (distance fr to) < 13.125) ==> let m = moveFromTo fr to (distance fr to) in m !$ fr =!!= to

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

getPointToOxyAroundOxToOxy a =  not (isNaN (newz/newt)) && (newz/newt) =.= 0  where Point _ newy newz newt = getPointToOxyAroundOx a !$ a
getPointToOxzAroundOzToOxz a =  not (isNaN (newy/newt)) &&  (newy/newt) =.= 0  where Point _ newy newz newt = getPointToOxzAroundOz a !$ a


getPointToOxToOx a = (newy/newt) =.= 0 .&&.  (newz/newt) =.= 0 where Point _ newy newz newt = (getPointToOxyAroundOx `andThen` getPointToOxzAroundOz $ a) !$ a
getPointOnOxToOriginToOrigin a b = let p = (Point a 0 0 b) in  (proper p) ==> getPointOnOxToOrigin p !$ p =!!= origin
getPointToOriginToOrigin a = (   getPointToOxyAroundOx `andThen` 
                                 getPointToOxzAroundOz `andThen` 
                                 getPointOnOxToOrigin $ a) !$ a =!!= origin


isometryGetTr a b c = isometry (getTriangleToOxy  a  b c)
aToOGetTr a b c = getTriangleToOxy a b c !$ a =!!!= origin
bToOxGetTr a b c = not (isNaN (newx/newt)) .&&. not (isNaN (newy/newt)) .&&. not (isNaN (newz/newt)) .&&. (newx/newt) >= 0 .&&. (newy/newt) =.= 0 .&&. (newz/newt) =.= 0  where Point newx newy newz newt = getTriangleToOxy a b c !$ b
cToOxyGetTr a b c = not (isNaN (newx/newt)) .&&. not (isNaN (newy/newt)) .&&. not (isNaN (newz/newt)) .&&. (newy/newt) >= 0 .&&. (newz/newt) =.= 0  where Point newx newy newz newt = getTriangleToOxy a b c !$ c

getTr a b c = isometryGetTr a b c .&&. aToOGetTr a b c .&&. bToOxGetTr a b c .&&. cToOxyGetTr a b c 

main = verboseCheck (isometry . moveAlongX )

check = verboseCheckWith (stdArgs {maxDiscardRatio = 100000, maxSuccess = 200})
