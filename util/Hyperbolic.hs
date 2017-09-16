{-# Language NoMonomorphismRestriction, OverloadedStrings,
             MultiParamTypeClasses, DeriveFunctor, DeriveGeneric, ScopedTypeVariables, BangPatterns, Strict #-}
-- module Hyperbolic (Point(..), form, formV, moveAlongX, moveAlongY, moveAlongZ, _v4, _t,
--                    rotateAroundZ, rotateAroundY, rotateAroundX, origin, insanity, identityIm, 
--                    pretty, proper, distance, chDistance, adj44, moveTo, invAroundZ, invAroundY, fromV4, toV4 {-FIMXE when learn lens-}, 
--                    normalizeWass, normalizeKlein, Movable (..), transposeMink, turmPToOxz, turmPToOx, moveFromTo, commute,
--                    moveRightTo, getTriangleToOxy) where


module Hyperbolic  where -- x=
   
import Data.Monoid
import GHC.Generics (Generic1) 
import Data.Foldable
import Data.List (intercalate) 

import qualified Linear as L
import Linear ((!*!))
import qualified Control.Lens as Lens

toV4 :: Point a -> L.V4 a
toV4 (Point a b c d) = L.V4 a b c d {-FIMXE when learn lens-}
{-# INLINE toV4 #-}
{-|

Этот модуль описывает гиперболическое пространство
So i cant switch to russian so i'm going to try to write comments in english with mistakes

There are three ways to code m-dimentional subspace of E^n: with m points it contains, 
 with n-m linear equations, and with grassman method.

Two first ways are very similar and dual to each other in a sense. Neither of them is better.
When seen like maps Gr(m, n) -> R^(m*n) (or n-m for second approach, i'll ignore this difference below)
 they lack bijectivity:  
image of this map contains matrices m*n of maximal rank and similar matrices represent same subspace.
Third way (grassman coordinates) lacks bijectivity as well: its image is order-2 hypersurface in codomain.

First approach is almost surjective: complement of its image is lower-dimensional.
Third approach is almost injective: two coordinates represent same subspace exactly when 
they are proportional.

Injectivity matters when comparing two subspaces. But surjectivity is way more important: it gaurantees 
that trere won't be any meaningless values of type 'subspace'.

For that reason i chose first way for 1- and 2-subspaces and second for 3-subspaces.
They are also easier to implement (not that it's important or anything).

Для переключения между первыми двумя подходами нужно решить систему уравнений
-}
{- |
Klein's approach for representing hyperbolic space is used.
hyperbolic 3-space is area of projective 3-space inside an oval.
projective 3-space is sheaf in 4-dimensional vector space.

На самом деле пространство у нас трёхмерное, и для представления точки достаточно трёх координат x, y, z,
если использовать проекцию Ганса, которая является глобальным диффеоморфизмом между R^3 и H^3. 
При этом t считается как sqrt(1+x^2+y^2+z^2). К сожалению, такой подход потребует включения лишнего 
констрейнта (Floating  a) в большинство функций and overall inefficient.

Уровни абстракции расположены так:
                4-мерное пространство
                /                   \
              |/                     \|
               --                   --
Пространство Минковского    Проективная плоскость
               \                       /
                \|                   |/
               --                     -- 
Пространство Лобачевского (с идеальными элементами)

В правой системе координат если Ox направлена  вперёд, а Oz вверх, то Oy направлена влево!
-} 

instance L.Additive Point where

data Point a = Point !a !a !a !a deriving (Generic1, Show, Eq, Functor, Read)
{- ^ for proper point x^2 + y^2 + z^2 - t^2 < 0 so this map 
  is not nearly injective, that's sad. However, sometimes i use improper points -}
_v4::Lens.Lens' (Point a) (L.V4 a)
_v4 f (Point x y z t) = fmap fromV4 $ f (L.V4 x y z t)

_t :: Lens.Lens' (Point a) (a)
_t f (Point a b c d) = Point a b c <$> f d
{-# INLINE _t #-}

--m !$ p = over _v4 (m!*) p
--agressive inlining...
fromV4 :: L.V4 a -> Point a
fromV4 (L.V4 a s d f) = Point a s d f

klein :: Fractional a => Point a -> L.V3 a
klein (Point x y z t) = L.V3 (x/t) (y/t) (z/t) 

instance Applicative Point where --stupid instance, don't use it
  pure a = Point a a a a
  (Point a b c d) <*> (Point e f g h) = Point (a e) (b f) (c g) (d h)

tau :: Floating a => a
tau = 2 * pi

-- |4x4 matrix transpose with respect to minkowski form
-- for hyperbolic space isometries this is same as inL.V44 (modulo floating-point precision)
transposeMink::Num a => L.M44 a -> L.M44 a
transposeMink (L.V4 (L.V4 a b c d) (L.V4 e f g h) (L.V4 i j k l) (L.V4 m n o p))
  = (L.V4 (L.V4 a e i (-m)) (L.V4 b f j (-n)) (L.V4 c g k (-o)) (L.V4 (-d) (-h) (-l) p))

sanity :: Num a => L.M44 a -> L.M44 a
sanity a = a !*! transposeMink a

insanity::Num a => L.M44 a -> a
insanity a = getSum $ fold $ fmap (foldMap (\x->Sum $ x*x)) (sanity a L.^-^ L.identity)

form :: Num a => Point a -> Point a -> a {- fundamental minkowski form, она зависит от координатного
представления точки, то есть не однозначна для точек гиперболического пространства, 
её стоит использовать с осторожностью -}
form (Point x1 y1 z1 t1) (Point x2 y2 z2 t2) = x1*x2 + y1*y2 + z1*z2 - t1*t2 

formV :: Num a => L.V4 a -> L.V4 a -> a
formV (L.V4 x1 y1 z1 t1) (L.V4 x2 y2 z2 t2) = x1*x2 + y1*y2 + z1*z2 - t1*t2 

-- next three datatypes represent things in hyperbolic space
-- data Line a = Line (Point a) (Point a) {- ^ if one of the points is proper, the line is proper -}

data Plane a = Plane (Point a) (Point a) (Point a) {- ^ if one of the points is proper,
                                                          the plane is proper -}
-- data Plane a = Plane a a a a {- ^ for proper plane a^2 + b^2 + c^2 - d^2 > 0 -}
   
data Absolute a = Abs a a a {- ^ point on celestial sphere or "absolute point". t^2 = x^2 + y^2 + z^2 
x^2+y^2+z^2 > 0-}



type ProjectiveMap a = L.V4 (L.V4 a)

reflectAboutOrigin :: Num a => L.M44 a
reflectAboutOrigin = L.V4 (L.V4 (-1) 0 0 0) (L.V4 0 (-1) 0 0) (L.V4 0 0 (-1) 0) (L.V4 0 0 0 1)

moveAlongX, moveAlongY, moveAlongZ :: Floating a => a -> L.M44 a
-- По дебильному решению кметта матрицы записываются по строкам :(
moveAlongZ d = L.V4 (L.V4 1 0 0 0) (L.V4 0 1 0 0) (L.V4 0 0 (cosh d) (sinh d)) (L.V4 0 0 (sinh d) (cosh d))
moveAlongY d = L.V4 (L.V4 1 0 0 0) (L.V4 0 (cosh d) 0 (sinh d)) (L.V4 0 0 1 0) (L.V4 0 (sinh d) 0 (cosh d))
moveAlongX d = L.V4 (L.V4 (cosh d) 0 0 (sinh d)) (L.V4 0 1 0 0) (L.V4 0 0 1 0) (L.V4 (sinh d) 0 0 (cosh d))
rotateAroundX, rotateAroundY, rotateAroundZ :: Floating a => a -> L.M44 a 
rotateAroundZ a = L.V4 (L.V4 (cos a) (-sin a) 0 0) (L.V4 (sin a) (cos a) 0 0) (L.V4 0 0 1 0) (L.V4 0 0 0 1)
rotateAroundY a = L.V4 (L.V4 (cos a) 0 (sin a) 0) (L.V4 0 1 0 0) (L.V4 (-sin a) 0 (cos a) 0) (L.V4 0 0 0 1)
rotateAroundX a = L.V4 (L.V4 1 0 0 0) (L.V4 0 (cos a) (-sin a) 0) (L.V4 0 (sin a) (cos a) 0) (L.V4 0 0 0 1)

moveAlongX3, moveAlongY3 :: Floating a => a -> L.M33 a
moveAlongY3 d = L.V3 (L.V3 1 0 0) (L.V3 0 (cosh d)  (sinh d)) (L.V3 0 (sinh d) (cosh d))
moveAlongX3 d = L.V3 (L.V3 (cosh d) 0 (sinh d)) (L.V3 0 1 0) (L.V3 (sinh d) 0 (cosh d))
rotate3 :: Floating a => a -> L.M33 a
rotate3 a = L.V3 (L.V3 (cos a) (-sin a) 0) (L.V3 (sin a) (cos a) 0) (L.V3 0 0 1)
rotateAround3 :: RealFloat a => L.V3 a -> a -> L.M33 a
rotateAround3 p a = commute3 (transposeMink3 $ moveRightTo3 p) (rotate3 a)
reflectOnX3 :: Num a => L.M33 a
reflectOnX3 = L.V3 (L.V3 (-1) (0) 0) (L.V3 (0) (1) 0) (L.V3 0 0 1)
reflectOnY3 :: Num a => L.M33 a
reflectOnY3 = L.V3 (L.V3 (1) (0) 0) (L.V3 (0) (-1) 0) (L.V3 0 0 1)
reflect3 :: RealFloat a => L.V3 a -> L.V3 a -> L.M33 a
reflect3 p1 p2 = commute3 (getSegmentToOx3 p1 p2) reflectOnX3 

moveToTangentVector :: RealFloat a => L.V3 a -> L.M44 a
moveToTangentVector v@(L.V3 x y z) = moveRightTo $ Point (cosh x) (cosh y) (cosh z) (sinh (L.norm v)) -- fixme это вроде неправильно
moveToTangentVector3 :: RealFloat a => L.V2 a -> L.M33 a
moveToTangentVector3 v@(L.V2 x y) = moveTo3  (L.V3 (x) (y) (sqrt (x*x + y*y + 1))) (L.norm v) 
 
origin3 :: Num a => L.V3 a
origin3 = L.V3 0 0 1
distance3 :: Floating a => L.V3 a -> L.V3 a -> a
distance3 p1 p2 = acosh (form3 (normalizeWass3 p1) (normalizeWass3 p2))
transposeMink3 :: Num a => L.M33 a -> L.M33 a
transposeMink3 (L.V3 (L.V3 q w e) (L.V3 r t y) (L.V3 u i o)) = L.V3 (L.V3 q r (-u)) (L.V3 w t (-i)) (L.V3 (-e) (-y) o)
normalizeWass3 :: Floating a => L.V3 a -> L.V3 a
normalizeWass3 (L.V3 x y t) = L.V3 (x/d) (y/d) (t/d) where d = sqrt (t*t - y*y - x*x) * signum t
normalizeKlein3 :: Floating a => L.V3 a -> L.V3 a
normalizeKlein3 (L.V3 x y t) = L.V3 (x/t) (y/t) 1

form3 :: Num a => L.V3 a -> L.V3 a -> a
form3 (L.V3 x1 y1 z1) (L.V3 x2 y2 z2) = z1*z2 - x1*x2 - y1*y2


moveRightFromTo3 :: RealFloat a => L.V3 a -> L.V3 a -> L.M33 a
moveRightFromTo3 p1 p2 = moveRightTo3 p1 !*! moveRightTo3 (transposeMink3 (moveRightTo3 p1) L.!* p2) !*! transposeMink3 ( moveRightTo3 p1)
 
qr :: Floating a => L.M44 a -> L.M44 a
qr {-(L.V4 (L.V4 a11 a12 a13 a14) 
         (L.V4 a21 a22 a23 a24) 
         (L.V4 a31 a32 a33 a34) 
         (L.V4 a41 a42 a43 a44) )-} = error "qr is not implemented"


-- fast special invertions:
-- (add inline pragmas...)
invAroundY, invAroundZ :: (Num a) => L.M44 a -> L.M44 a
invAroundZ (L.V4 (L.V4 a b _ _) _ _ _) = L.V4 (L.V4 a (-b) 0 0) (L.V4 b a 0 0) (L.V4 0 0 1 0) (L.V4 0 0 0 1)
invAroundY (L.V4 (L.V4 a _ b _) _ _ _) = L.V4 (L.V4 a 0 (-b) 0) (L.V4 0 1 0 0) (L.V4 b 0 a 0) (L.V4 0 0 0 1)

-- |4x4 matrix adjugate (inverse multiplied by det)
-- for matrices with det 1 this is same as inL.V44 (modulo floating-point precision)
-- (copied and patsed from linear package)
adj44 :: Fractional a => L.M44 a -> L.M44 a
adj44   (L.V4 (L.V4 i00 i01 i02 i03)
            (L.V4 i10 i11 i12 i13)
            (L.V4 i20 i21 i22 i23)
            (L.V4 i30 i31 i32 i33)) =
  let s0 = i00 * i11 - i10 * i01
      s1 = i00 * i12 - i10 * i02
      s2 = i00 * i13 - i10 * i03
      s3 = i01 * i12 - i11 * i02
      s4 = i01 * i13 - i11 * i03
      s5 = i02 * i13 - i12 * i03
      c5 = i22 * i33 - i32 * i23
      c4 = i21 * i33 - i31 * i23
      c3 = i21 * i32 - i31 * i22
      c2 = i20 * i33 - i30 * i23
      c1 = i20 * i32 - i30 * i22
      c0 = i20 * i31 - i30 * i21
  in L.V4 (L.V4 (i11 * c5 - i12 * c4 + i13 * c3)
           (-i01 * c5 + i02 * c4 - i03 * c3)
           (i31 * s5 - i32 * s4 + i33 * s3)
           (-i21 * s5 + i22 * s4 - i23 * s3))
        (L.V4 (-i10 * c5 + i12 * c2 - i13 * c1)
           (i00 * c5 - i02 * c2 + i03 * c1)
           (-i30 * s5 + i32 * s2 - i33 * s1)
           (i20 * s5 - i22 * s2 + i23 * s1))
        (L.V4 (i10 * c4 - i11 * c2 + i13 * c0)
           (-i00 * c4 + i01 * c2 - i03 * c0)
           (i30 * s4 - i31 * s2 + i33 * s0)
           (-i20 * s4 + i21 * s2 - i23 * s0))
        (L.V4 (-i10 * c3 + i11 * c1 - i12 * c0)
           (i00 * c3 - i01 * c1 + i02 * c0)
           (-i30 * s3 + i31 * s1 - i32 * s0)
           (i20 * s3 - i21 * s1 + i22 * s0))
{-# INLINE adj44 #-}

{-

*Main> distance (Point (-1.7252007297487297) (-14.389920392888179) 7.890168332835287 17.402341677747398) (Point (-1.7252007297487297) (-14.389920392888179) 7.890168332835287 17.402341677747398)
NaN
*Main> chDistance (Point (-1.7252007297487297) (-14.389920392888179) 7.890168332835287 17.402341677747398) (Point (-1.7252007297487297) (-14.389920392888179) 7.890168332835287 17.402341677747398)
0.9999999999999982
*Main> 
jjjjjgsgdxxxxxxx
fd
-}
distance :: (Floating a, Ord a) => Point a -> Point a -> a
distance a b 
  | (chDistance a b) > 1 = acosh (chDistance a b)
  | otherwise = 0 


legByAdjacentAndOppositeAngles :: (Floating a) => a -> a -> a
legByAdjacentAndOppositeAngles a b = acosh $ cos b / sin a

hypotenuseByAngles :: (Floating a) => a -> a -> a
hypotenuseByAngles a b = acosh $ recip $ tan a * tan b

chDistance :: Floating a => Point a -> Point a -> a
chDistance a b = negate (form (normalizeWass a) (normalizeWass b)) --let diff = a ^-^ b in form diff diff
signedDistanceFromOxy :: (Floating a, Ord a) => Point a -> a
signedDistanceFromOxy p@(Point x y z t) = distance p (Point x y 0 t) * signum z * signum t
-- that is, they must be linearly dependent
origin :: Num a => Point a
origin = Point 0 0 0 1

pretty :: Show m => L.V4 (m) -> String
pretty (L.V4 a b c d) = intercalate "\n" $ map show [a, b, c, d]

proper :: (Num a, Ord a) => Point a -> Bool
proper m = form m m < 0

normalizeKlein :: Fractional a => Point a -> Point a
normalizeKlein (Point x y z t) = Point (x/t) (y/t) (z/t) 1
normalizeWass :: Floating a => Point a -> Point a
normalizeWass (Point x y z t) = Point (x/d) (y/d) (z/d) (t/d)
    where d = sqrt ((-(x*x)) - y*y - z*z + t*t) * signum t

rotate :: (RealFloat a) => Point a -> Point a -> a -> L.M44 a
rotate a b x = commute ((getPointToOrigin a `andConsideringThat` turmPToOx) b) (rotateAroundX x)

commute :: Num a => L.M44 a -> L.V4 (L.V4 a) -> L.V4 (L.V4 a)
commute a b = (transposeMink a) !*! b !*! a
commute3 :: Num a => L.M33 a -> L.V3 (L.V3 a) -> L.V3 (L.V3 a)
commute3 a b = (transposeMink3 a) !*! b !*! a

moveTo :: (RealFloat a) => Point a -> a -> L.M44 a
moveTo p dist =  transposeMink a !*! moveAlongX (dist) !*! a
    where a = (getPointToOxyAroundOx `andThen`  getPointToOxzAroundOz)  p
--moveRightTo3 p@(L.V3 x y t) = rotate3 ((atan2 y x)) !*! moveAlongX3 (distance3 origin3 p) !*! rotate3 (-(atan2 y x))
moveTo3 :: RealFloat a =>  L.V3 a -> a -> L.M33 a
moveTo3 (L.V3 x y _) d = rotate3 ((atan2 y x)) !*! moveAlongX3 (d) !*! rotate3 (-(atan2 y x))

moveRightTo :: RealFloat a => Point a -> L.M44 a
moveRightTo p = moveTo p (distance origin p) 

moveRightTo3 :: RealFloat a => L.V3 a -> L.M33 a
moveRightTo3 p = moveTo3 p (distance3 origin3 p) 
    
moveFromTo :: (RealFloat a) => Point a -> Point a -> a -> L.M44 a
moveFromTo fr to dist =   a !*! moveTo (transposeMink a!$to) (dist) !*! transposeMink a
    where a = moveRightTo fr --может быть, тут можно вместо moverightto использовать более простое движение - не из пяти, а из трёх элементарных

turmPToOxz ::forall a . (Eq a, Floating a) => Point a -> L.M44 a -- cbc fixme this function is same as getPointToOxzAroundOz ??
turmPToOxz  (Point x y _ t) = rotateAroundZ alpha
  where alpha = if(x/=0)then atan (-y/x) + ((signum (x*t) - 1)/2) * (-pi) else 0

turmPToOx ::forall a . (Eq a, Floating a) => Point a -> L.M44 a -- cbc fixme this function is same as getPointToOxyAroundOx ??
turmPToOx  (Point x y z t) =  rotateAroundY (beta) !*! rotateAroundZ alpha
  where alpha = if(x/=0)then atan (-y/x) + ((signum (x*t) - 1)/2) * (-pi) else 0
        beta = -signum t * (if (x /= 0 ) || (y /= 0) then atan (-z/sqrt(x*x + y*y)) else 0)
-- тут сожно наверное всё сделать ДРАМАТИЧЕСКИ быстрее, если вставить rewrite rules

--atan3 y x t = atan2 y x 
m33_to_m44M :: Num a => L.M33 a -> L.M44 a
m33_to_m44M (L.V3 (L.V3 q w e) (L.V3 r t y) (L.V3 u i o)) = L.V4 (L.V4 q w 0 e) (L.V4 r t 0 y) (L.V4 0 0 1 0) (L.V4 u i 0 o)


getPointToOrigin, getPointToOxzAroundOz, getPointToOxyAroundOx, getPointOnOxToOrigin :: forall a. RealFloat a => Point a -> L.M44 a
getPointToOrigin = transposeMink . moveRightTo
getPointToOxzAroundOz (Point x y _ t) = rotateAroundZ $ -(atan2 (y/t) (x/t)) --  брать синус и косинус арктангенса очень весело, конечно
getPointToOxyAroundOx (Point _ y z t) = rotateAroundX $ -(atan2 (z/t) (y/t)) -- от t нам нужен только знак, конечно, но я подозреваю, что лишний флоп лучше, чем лишнее ветвление
getPointOnOxToOrigin (Point x _ _ t) = moveAlongX $ asinh $ (  - x/ sqrt (( (t*t-x*x))) * signum t) -- брать гиперболические синус и косинус аркчосинуса очень весело, конечно
-- moveFromTo a b d = 

andThen :: (Num a, Movable m) => (m a -> L.M44 a) -> (m a -> L.M44 a) -> m a -> L.M44 a -- может быть, это какой-нибудь arrows 
((!f) `andThen` (!g)) (!p) = g ((f p) !$ p) !*! f p -- безумно неэффективно и вообще пиздец

((!f) `andThen3` (!g)) (!p) = g ((f p) L.!* p) L.!*! f p

andConsideringThat :: (Num a, Movable m) => L.M44 a -> (m a -> L.M44 a) -> m a -> L.M44 a 
andConsideringThat !m !f !p = f (m !$ p) !*! m

andConsideringThat3 !m !f !p = f (m L.!* p) L.!*! m

-- getTriangleToOxy !a !b !c = (   (   getPointToOxyAroundOx `andThen` 
--                                  getPointToOxzAroundOz `andThen` 
--                                  getPointOnOxToOrigin $ a) `andConsideringThat` 
--                              (   getPointToOxyAroundOx `andThen` 
--                                  getPointToOxzAroundOz ) ) b `andConsideringThat` 
--                          getPointToOxyAroundOx $! c
getTriangleToOxy :: forall a.
                          RealFloat a =>
                          Point a -> Point a -> Point a -> L.M44 a
getTriangleToOxy !a !b !c = (   (   getPointToOxyAroundOx `andThen` 
                                 getPointToOxzAroundOz `andThen` 
                                 getPointOnOxToOrigin $ a) `andConsideringThat` 
                             (   getPointToOxyAroundOx `andThen` 
                                 getPointToOxzAroundOz ) ) b `andConsideringThat` 
                         getPointToOxyAroundOx $! c
getTriangleToOxyD :: forall a.
                           RealFloat a =>
                           Point a -> Point a -> Point a -> (Point a, Point a, Point a)
getTriangleToOxyD !a !b !c = (t !$ a, t !$ b, t !$ c) where t = (getTriangleToOxy a b c) `seq` (getTriangleToOxy a b c)

getSegmentToOx3 :: RealFloat a => L.V3 a -> L.V3 a -> L.M33 a
getSegmentToOx3 a b = getPointToOrigin3 a `andConsideringThat3` getPointToOxAroundO3 $ b
getPointToOrigin3 = transposeMink3 . moveRightTo3
getPointToOxAroundO3 (L.V3 x y t) = rotate3 $ -(atan2 (y/t) (x/t))
{- 
Мы хотим сделать, чтобы все углы треугольника стали на од




-}


class Movable p where
  (!$) :: (Num a) => L.M44 a -> p a -> p a 
infixr 1 !$
instance Movable Point where
  m !$ p = Lens.over _v4 (m L.!*) p
--so much for agressive inlining... class methods dont get rewrited... class methods are harder to inline.. sad..


