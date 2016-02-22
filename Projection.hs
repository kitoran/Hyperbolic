{-# Language NoMonomorphismRestriction, OverloadedStrings #-}
module Projection(viewPoint, 
                    Projector (..)) where
import Hyperbolic
import Linear.Matrix
import Linear.V4
import Linear.V3
data Projector a = Projector {
                    projectorPosition::(Point a),
                    projectorDirection::(Point a),
                    projectorHorizontal::(Point a),
                    projectorVertical::(Point a)
                     }
{- ^ Viewer's eye, direction and SOME vertical
direction.
Note that second absolute point directs up but not nessesarily straight up; it doesn't need to be
orthogonal to first one 
Положение точки d определяет масштаб полученного изображения. Благодаря этому я могу не извлекать
корень при проецировании, но должен извлечь его, когда устанавливаю камеру 


-

class Viewable e i where {- ^ entity e is viewed as i -}
    view :: {- some constraint on a -} Camera a -> e a -> i a



-

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

toV4 (Point a b c d) = V4 a b c d

{-
instance Viewable Point where
    view c = (toAffine origin (translate polarPlane d)) . (project p0 planeOfView) 
        where planeOfView = Plane d a b
              Line a b = eqtobas (LineEq (polar p0) (polar d))
              polarPlane (Point x y x t) = Plane (-x) (-y) (-z) t
              --project 
              -}

toAffineDeprecated :: Fractional a => Point a -> (Point a -> V3 a)
toAffineDeprecated p0@(Point x0 y0 z0 t0) p@(Point x y z t) 
    = V3 ((t' *x - x0)/t0) ((t' *y - y0)/t0) ((t' *z - z0)/t0)
        where t' = form p0 p / form p0 p0


decompose :: Fractional a => V4 (V4 a) -> V4 a -> (V4 a)
decompose m p = inv44 m !* p

viewPoint :: Fractional a => Projector a -> Point a -> (a, a)
viewPoint c p = (h / (t * d), v / (t * d)) 
    where V4 h v d t = decompose matrix (toV4 p) 
          Projector p0'' d'' h'' v'' = c
          [p0', d', h', v'] = map toV4 [p0'', d'', h'', v'']
          matrix = V4 (h' - p0') (v' - p0') (d' - p0') (p0')

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
