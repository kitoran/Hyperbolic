{-# Language NoMonomorphismRestriction, OverloadedStrings #-}
module Projection where
import Hyperbolic

data Camera a = Cam {
                    cameraPosition::(Point a),
                    cameraDirection::(Absolute a),
                    cameraVerticalDirection::(Absolute a)
                     }
{- ^ Viewer's eye, direction and SOME vertical
direction.
Note that second absolute point directs up but not nessesarily straight up; it doesn't need to be
orthogonal to first one  -}

class Viewable e i where {- ^ entity e is viewed as i -}
    view :: {- some constraint on a -} Camera a -> e a -> i a



{-

Чтобы спроецировать точку на плоскость, мы проецируем её на абсолют и затем проецируем её
на плоскость, касающуюся абсолюта в точке cameraDirection из точки cameraPosition

Проецирование точки на абсолют is pretty straightforward, но спроецировать точку абсолюта на касательную
плоскость можно разными способами (с одинаковым результатом). Я реализовал первый способ, пришедший в
голову: использовать простое линейное преобразование, переводящее cameraPosition в центр (точку 0,0,0,t)
и не меняющее t. При этом, конечно, метрика Минковского летит к хуям.

-}

direction :: Floating a => Point a -> Point a -> Absolute a
{- ^ Эта функция проецирует вторую точку из первой на абсолют.
 Может быть, её можно написать без констрейнта -}
direction a b = a + c*b
    where c = (ab + sqrt (ab*ab - aa*bb))/aa
          ab = form a b
          bb = form b b
          aa = form a a

-- функция tangentHyperplane, наверное, даже не понадобится
tangentHyperplane :: Floating a => Absolute a -> Plane a
tangentHyperplane (Abs x y z) = (Plane (-x) (-y) (-z) (sqrt $ 1+x*x+y*y+z*z))

moveHyperplane :: Absolute a -> Plane a -> Plane a
moveHyperplane (Abs x y z) (Plane a a a a) = 

data Point2 a = Point2 a a

data Segment2 a = Seg2 (Point2 a) (Point2 a)

data PlaneImage2 a = PI2 (Point2 a) a a a a {- ^ center, direction of main axis and axes -}
 {- not all ellipses are given this way; this type needs refactoring    -}

--instance Viewable Point Point2 where
--    view (Cam (Point cx cy cz ct) (Abs tx ty tz) (Abs vx vy vz)) (Point x y z t) =
