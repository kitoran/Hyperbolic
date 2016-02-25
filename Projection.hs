{-# Language NoMonomorphismRestriction, OverloadedStrings #-}
module Projection(viewPoint, 
                    Projector (..)) where
import Hyperbolic
import Linear.Matrix hiding (inv33)
import Linear.V4
import Linear.Vector
import Linear.V3
-------- for inv33
import Linear.V2
--------
data Projector a = Projector {
                    projectorPosition::(Point a),
                    projectorDirection::(Point a),
                    projectorHorizontal::(Point a),
                    projectorVertical::(Point a)
                     }
{- this function is defined in Matrix with stronger constraint -}
inv33 :: Fractional a => M33 a -> M33 a
inv33 m@(V3 (V3 a b c)
            (V3 d e f)
            (V3 g h i))
  = (1 / det) *!! V3 (V3 a' b' c')
                     (V3 d' e' f')
                     (V3 g' h' i')
  where a' = cofactor (e,f,h,i)
        b' = cofactor (c,b,i,h)
        c' = cofactor (b,c,e,f)
        d' = cofactor (f,d,i,g)
        e' = cofactor (a,c,g,i)
        f' = cofactor (c,a,f,d)
        g' = cofactor (d,e,g,h)
        h' = cofactor (b,a,h,g)
        i' = cofactor (a,b,d,e)
        cofactor (q,r,s,t) = det22 (V2 (V2 q r) (V2 s t))
        det = det33 m
{-# INLINE inv33 #-}
{- -}


{- ^ Viewer's eye, direction and SOME vertical
direction.
projectorHorizontal проецируется в (0, 1), projectorVertical - в (1, 0)


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


decompose :: Fractional a => V3 (V3 a) -> V3 a -> (V3 a)
decompose m p = {-(-p *!) -}transpose (inv33 (transpose m))  !* p

viewPoint :: Fractional a => Projector a -> Point a -> (a, a)
viewPoint (Projector p0 d h v) p = (x, y) where
  [h', v', d', p0', p'] = map (normalizePoint . toV4) [h, v, d, p0, p]
  [h'', v'', d'', p''] = map (\t -> t-p0') [h', v', d', p']
  matrix = V3 (h'' ^-^ d'') (v'' ^-^ d'') (d'')
  V3 x y _ = decompose matrix p''
--не совсем понятно мне, как писать без этих вот dash'ей. State вроде не позволяет так легко
--сопоставлять и мапить состояния.
  {- я не понимаю, почему эта версия viewPoint работает, поэтому удаляю её
viewPoint :: Fractional a => Projector a -> Point a -> (a, a)
viewPoint c p = 
    where V4 h v d t = decompose matrix $ (form p0'' p0'' / form p0'' p) *^  toV4 p
          Projector p0'' d'' h'' v'' = c
          [p0', d', h', v'] = map toV4 [p0'', d'', h'', v'']
          matrix = V4 (ch *^ h' ^-^  cd *^ d') (cv *^ v' ^-^ p0' ) ( cd *^ d' ^-^ p0' ) (p0')
          [ch, cv, cd] = [form p0'' p0'' / form p0'' t| t <- [h'', v'', d'']]
-}
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
