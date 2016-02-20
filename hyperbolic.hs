{-# Language NoMonomorphismRestriction, OverloadedStrings, MultiParamTypeClasses, DeriveFunctor, DeriveGeneric #-}
module Hyperbolic where

import Linear.Vector
import Control.Applicative
import GHC.Generics
{-|

Этот модуль описывает гиперболическое пространство
So i cant switch to russian so i'm going to try to write comments in english with mistakes

There are three ways to code m-dimentional subspace of E^n: with m points it contains, 
 with n-m linear equations, and with grassman method.

First two ways are very similar and dual to each other in a sense. Neither of them is better.
When seen like maps Gr(m, n) -> R^(m*n) (or n-m for second approach, i'll ignore this difference below)
 they lack bijectivity:  
image of this map contains matrices m*n of maximal rank and similar matrices represent same subspace.
Third way (grassman coordinates) lacks bijectivity as well

First approach is almost surjective: complement of its image is lower-dimensional.
Third approach is almost injective: two coordinates represent same subspace exactly when 
they are proportional.

Injectivity matters when comparing two subspaces. But surjectivity is way more important: it gaurantees 
that trere won't be any meaningless values of type 'subspace'.

For that reason i chose first way for 1- and 2-subspaces and second for 3-subspaces.
They are also easier to implement (not that it's important).
-}
{- |
Klein's approach for representing hyperbolic space is used.
hyperbolic 3-space is area of projective 3-space inside an oval.
projective 3-space is sheaf in 4-dimensional vector space.

На самом деле пространство у нас трёхмерное, и для представления точки достаточно трёх координат x, y, z,
если использовать проекцию Ганса, которая является глобальным диффеоморфизмом между R^3 и H^3. 
При этом t считается как sqrt(1+x^2+y^2+z^2). К сожалению, такой подход потребует включения лишнего 
констрейнта (Floating a) в большинство функций.

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


data Point a = Point a a a a deriving (Generic1)
{- ^ for proper point x^2 + y^2 + z^2 - t^2 < 0 so this map 
  is not nearly injective, that's sad. However, sometimes i use improper points -}
instance Applicative Point where -- This instance is needed for Additive instance
-- it's wrong and should not be exported
    pure a = Point a a a a
    (Point a b c d) <*> (Point e f g h) = Point (a e) (b f) (c g) (d h)



form :: Num a => Point a -> Point a -> a {- fundamental minkowski form, она зависит от координатного
представления точки, то есть не инвариантна для точек гиперболического пространства, 
её стоит использовать с осторожностью -}
form (Point x1 y1 z1 t1) (Point x2 y2 z2 t2) = x1*x2 + y1*y2 + z1*z2 - t1*t2 

-- next three datatypes represent things in hyperbolic space
data Line a = Line (Point a) (Point a) {- ^ if one of the points is proper, the line is proper -}

data Plane a = Plane a a a a {- ^ for proper plane a^2 + b^2 + c^2 - d^2 > 0 -}
   
data Absolute a = Abs a a a {- ^ point on celestial sphere or "absolute point". t^2 = x^2 + y^2 + z^2 
x^2+y^2+z^2 > 0-}




