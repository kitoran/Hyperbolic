{-# Language NoMonomorphismRestriction, OverloadedStrings,
             MultiParamTypeClasses, DeriveFunctor, DeriveGeneric #-}
module Hyperbolic (Point(..), form, formV, moveAlongX, moveAlongY, moveAlongZ, _v4,
                   rotateAroundZ, rotateAroundY, rotateAroundX, origin, insanity, identityIm, pretty) where

import Linear hiding (transpose)
import Control.Lens
import Control.Applicative
import Data.Monoid
import Data.Foldable
import Data.List (intercalate)
import GHC.Generics

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
констрейнта (Floating a) в большинство функций and overall inefficient.

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

-}

instance Additive Point where
  
data Point a = Point !a !a !a !a deriving (Generic1, Show, Eq, Functor)
{- ^ for proper point x^2 + y^2 + z^2 - t^2 < 0 so this map 
  is not nearly injective, that's sad. However, sometimes i use improper points -}
_v4::Lens' (Point a) (V4 a)
_v4 f (Point x y z t) = fmap fromV4 $ f (V4 x y z t)
fromV4 (V4 a s d f) = Point a s d f
instance Applicative Point where --stupid instance, don't use it
  pure a = Point a a a a
  (Point a b c d) <*> (Point e f g h) = Point (a e) (b f) (c g) (d h)

transpose::Num a => M44 a -> M44 a
transpose (V4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o p))
  = (V4 (V4 a e i (-m)) (V4 b f j (-n)) (V4 c g k (-o)) (V4 (-d) (-h) (-l) p))
sanity :: Num a => M44 a -> M44 a
sanity a = a !*! transpose a
insanity a = getSum $ fold $ fmap (foldMap (\x->Sum $ x*x)) (sanity a ^-^ identity)
form :: Num a => Point a -> Point a -> a {- fundamental minkowski form, она зависит от координатного
представления точки, то есть не однозначна для точек гиперболического пространства, 
её стоит использовать с осторожностью -}
form (Point x1 y1 z1 t1) (Point x2 y2 z2 t2) = x1*x2 + y1*y2 + z1*z2 - t1*t2 
formV (V4 x1 y1 z1 t1) (V4 x2 y2 z2 t2) = x1*x2 + y1*y2 + z1*z2 - t1*t2 

-- next three datatypes represent things in hyperbolic space
data Line a = Line (Point a) (Point a) {- ^ if one of the points is proper, the line is proper -}

data Plane a = Plane (Point a) (Point a) (Point a) {- ^ if one of the points is proper,
                                                          the plane is proper -}
-- data Plane a = Plane a a a a {- ^ for proper plane a^2 + b^2 + c^2 - d^2 > 0 -}
   
data Absolute a = Abs a a a {- ^ point on celestial sphere or "absolute point". t^2 = x^2 + y^2 + z^2 
x^2+y^2+z^2 > 0-}



type ProjectiveMap a = V4 (V4 a)

-- По дебильному решению кметта матрицы записываются по строкам :(
-- всё это можно сделать наверное линзами _z, _t
moveAlongZ d = V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 (cosh d) (sinh d)) (V4 0 0 (sinh d) (cosh d))
moveAlongY d = V4 (V4 1 0 0 0) (V4 0 (cosh d) 0 (sinh d)) (V4 0 0 1 0) (V4 0 (sinh d) 0 (cosh d))
moveAlongX d = V4 (V4 (cosh d) 0 0 (sinh d)) (V4 0 1 0 0) (V4 0 0 1 0) (V4 (sinh d) 0 0 (cosh d))
rotateAroundZ a = V4 (V4 (cos a) (-sin a) 0 0) (V4 (sin a) (cos a) 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
rotateAroundY a = V4 (V4 (cos a) 0 (sin a) 0) (V4 0 1 0 0) (V4 (-sin a) 0 (cos a) 0) (V4 0 0 0 1)
rotateAroundX a = V4 (V4 1 0 0 0) (V4 0 (cos a) (-sin a) 0) (V4 0 (sin a) (cos a) 0) (V4 0 0 0 1)


distance :: Floating a => Point a -> Point a -> a
distance a b = acosh (chDistance a b)

chDistance ::Num a => Point a -> Point a -> a
chDistance a b = negate (form a b) --let diff = a ^-^ b in form diff diff

data Quadrilatheral a = QL (Point a) (Point a) (Point a) (Point a) -- points must lie in same plane
-- that is, they must be linearly dependent

origin = Point 0 0 0 1
identityIm = identity--V4 (V4 (0) 0 0 1)(V4 0 (0) 1 0 )(V4 0 1  (0) 0) (V4 (-1) 0 0 (0))

pretty :: Show m => V4 (m) -> String
pretty (V4 a b c d) = intercalate "\n" $ map show [a, b, c, d]
{-
So i can't find out how to do this properly and i'm too shy to go to mail lists so i'll just have to invent physics myself :(
The only shape is (irregular) hexahedron. 


-}



