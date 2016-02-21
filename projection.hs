{-# Language NoMonomorphismRestriction, OverloadedStrings #-}
module Projection where
import Hyperbolic

data Camera a = Cam {
                    cameraPosition::(Point a),
                    cameraDirection::(Point a),
                    cameraVerticalDirection::(Absolute a)
                     }
{- ^ Viewer's eye, direction and SOME vertical
direction.
Note that second absolute point directs up but not nessesarily straight up; it doesn't need to be
orthogonal to first one 
Положение точки d определяет масштаб полученного изображения. Благодаря этому я могу не извлекать
корень при проецировании, но должен извлечь его, когда устанавливаю камеру -}

class Viewable e i where {- ^ entity e is viewed as i -}
    view :: {- some constraint on a -} Camera a -> e a -> i a



{-

Чтобы спроецировать точку p на плоскость камерой Cam p0 d v, мы

проецируем её из p0 на (гипер)плоскость, перпендикулярную прямой p0-d в точке d
затем проецируем из начала координат на (гипер)плоскость, полярную точке p0, сдвинутую на что-нибудь
Пока она сдвигается на d

-}

instance Viewable Point where
	view c = (project origin (translate polarPlane d)) . (project p0 planeOfView) 
		where planeOfView = Plane d a b
			  Line a b = eqtobas (LineEq (polar p0) (polar d))
			  polarPlane (Point x y x t) = Plane (-x) (-y) (-z) t

projectPoint::Camera a -> Point a -> Point2 a
projectPoint c p = 
	project p to plane orthogonal to line p d in p translated by d where d = cameraPosition c




data Point2 a = Point2 a a

data Segment2 a = Seg2 (Point2 a) (Point2 a)

data PlaneImage2 a = PI2 (Point2 a) a a a a {- ^ center, direction of main axis and axes -}
 {- not all ellipses are given this way; this type needs refactoring    -}

--instance Viewable Point Point2 where
--    view (Cam (Point cx cy cz ct) (Abs tx ty tz) (Abs vx vy vz)) (Point x y z t) =
