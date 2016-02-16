{-# Language MultiParamTypeClasses, DeriveFunctor, DeriveGeneric #-}
module Space where

import Linear.Vector
import Control.Applicative
import GHC.Generics
{- |
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

Also projective 2-space is used when showing hyperbolic space from inside
-}


data Point a = Point a a a a deriving (Additive, Generic1)
{- ^ for proper point x^2 + y^2 + z^2 - t^2 < 0 so this map 
  is not nearly injective, that's sad. However, sometimes i use improper points -}
instance Applicative Point where -- This instance is needed of Additive instance
-- it's wrong and should not be exported
    pure a = Point a a a a
    (Point a b c d) <*> (Point e f g h) = Point (a e) (b f) (c g) (d h)



form :: Num a => Point a -> Point a -> a -- fundamental minkowski form
form (Point x1 y1 z1 t1) (Point x2 y2 z2 t2) = x1*x2 + y1*y2 + z1*z2 - t1*t2 


data Line a = Line (Point a) (Point a) {- ^ if one of the points is proper, the line is proper -}

data Plane a = Plane a a a a {- ^ for proper plane a^2 + b^2 + c^2 - d^2 > 0 -}

data Absolute a = Abs a a a {- ^ point on celestial sphere or "absolute point". t^2 = x^2 + y^2 + z^2 -}

data Camera a = Cam (Point a) (Absolute a) (Absolute a) {- ^ Viewer's eye, direction and SOME vertical 
direction.
Note that second absolute point directs up but not nessesarily straight up; it doesn't need to be 
orthogonal to first one 

Space is projected on a sphere or radius 1 and then sphere is moved to 
euclid space and projected to tangent space -}

class Viewable e i where {- entity e is viewed as i -}
    view :: {- some constraint on a -} Camera a -> e a -> i a

direction :: Floating a => Point a -> Point a -> Absolute a
direction a b = a + c*b
    where c = (ab + sqrt (ab*ab - aa*bb))/aa
          ab = form a b
          bb = form b b
          aa = form a a



data Point2 a = Point2 a a

data Segment2 a = Seg2 (Point2 a) (Point2 a)

data PlaneImage2 a = PI2 (Point2 a) a a a a {- ^ center, direction of main axis and axes -}
 {- not all ellipses are given this way; this type needs refactoring    -}

--instance Viewable Point Point2 where
--    view (Cam (Point cx cy cz ct) (Abs tx ty tz) (Abs vx vy vz)) (Point x y z t) =
