{-# Language NoMonomorphismRestriction, OverloadedStrings, RebindableSystax #-}
module Projection where
import Hyperbolic

data Projector a = Cam {
                    projectorPosition::(Point a),
                    projectorDirection::(Point a),
                    projectorVerticalDirection::(Absolute a),
                    projectorHorizontalDirection::(Absolute a)
                     }
{- ^ Viewer's eye, direction and SOME vertical
direction.
Note that second absolute point directs up but not nessesarily straight up; it doesn't need to be
orthogonal to first one 
Положение точки d определяет масштаб полученного изображения. Благодаря этому я могу не извлекать
корень при проецировании, но должен извлечь его, когда устанавливаю камеру 


-}

class Viewable e i where {- ^ entity e is viewed as i -}
    view :: {- some constraint on a -} Camera a -> e a -> i a



{-

Чтобы спроецировать точку p на плоскость камерой Cam p0 d v, мы
1) превращаем проективное пространство в аффинное, выбирая в качестве бесконечно удалённой
плоскости плоскость, полярную p0. Можно подумать про хороший базис в этом афф. пространстве.
Придумал: базисные векторы t0 0 0 x0, 0 t0 0 y0 и т д



2) проецируем из p0 на плоскость, перпендикулярную прямой p0-d. Надо доказать, что эта плоскость
будет перпендикулярной и в аффинном, и в гиперболическом смысле.




Устаревшая инструкция, проецировавшая точки на бесконечность:
проецируем её из p0 на (гипер)плоскость, перпендикулярную прямой p0-d в точке d
затем проецируем из начала координат на (гипер)плоскость, полярную точке p0, сдвинутую на что-нибудь
Пока она сдвигается на d

-}

instance Viewable Point where
    view c = (toAffine origin (translate polarPlane d)) . (project p0 planeOfView) 
        where planeOfView = Plane d a b
              Line a b = eqtobas (LineEq (polar p0) (polar d))
              polarPlane (Point x y x t) = Plane (-x) (-y) (-z) t
			  --project 

toAffine :: Num a => Point a -> (Point a -> (a, a, a))
toAffine p0@(Point x0 y0 z0 t0) p@(Point x y z t) 
    = ((t' *x - x0)/t0, (t' *y - y0)/t0, (t' *z - z0)/t0)
        where t' = form p0 p / form p0 p0


 
{-projectPoint::Camera a -> Point a -> Point2 a
projectPoint c p = 
	project p to plane orthogonal to line p d in p translated by d where d = cameraPosition c
-}



data Point2 a = Point2 a a

data Segment2 a = Seg2 (Point2 a) (Point2 a)

data PlaneImage2 a = PI2 (Point2 a) a a a a {- ^ center, direction of main axis and axes -}
 {- not all ellipses are given this way; this type needs refactoring    -}

--instance Viewable Point Point2 where
--    view (Cam (Point cx cy cz ct) (Abs tx ty tz) (Abs vx vy vz)) (Point x y z t) =