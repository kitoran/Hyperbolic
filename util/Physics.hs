{-# Language TemplateHaskell, ScopedTypeVariables, NoMonomorphismRestriction, DataKinds, DuplicateRecordFields, DeriveFunctor, BangPatterns #-}
module Physics where

import qualified Hyperbolic as H
import Hyperbolic (Point(Point), (!$), tau)
import qualified Unsafe.Coerce
import qualified Debug.Trace
import qualified GHC.Exts
import qualified Control.Lens as Lens
import Linear(M44, (!*!), (!*), (*!), normalizePoint, V3(..), M33)

--fixme этот файл, конечно, надо переместить в util, а Mesh переместить в другой файл

--текущее положение  матрица куда надо пойти   результат (?) 
-- correct :: M44 Double -> M44 Double -> M44 Double
-- correct 
data State = State { _pos :: !(M33 Double )-- 
                   , _height :: !Double
                   , _nod :: !Double
                   , _speed :: !(V3 Double)
                   } deriving Show

$(Lens.makeLenses ''State)

currentPosition (State (!pos) (!height) _ _) =  H.m33_to_m44M pos !*! H.moveAlongZ height !$ H.origin
ourSize = 0.1

type Obstacles a = [Obstacle a]
data Obstacle a = Sphere !(Point a) a | Triangle !(Point a) !(Point a) !(Point a) !a deriving (Eq, Show,Functor, Read)
instance H.Movable Obstacle where
  (!tr) !$ (Sphere !p !r) = Sphere (tr !$ p) r
  (!tr) !$ (Triangle !q !w !e !r) = Triangle (tr !$ q) (tr !$ w) (tr !$ e) r

domainRadius :: Double
domainRadius = acosh (cosh al * cosh bl) 
  where a, b, c, al, bl :: Double
        a = tau/10
        b = tau/10
        c = tau/4
        al = acosh ( cos a / sin b )
        bl = acosh ( cos b / sin a )

-- breakDown :: Obstacles Double -> 




-- data Obstacle = OHCQ HorizontalCoordinateQuadrilateral deriving (Eq, Show)
data HorizontalCoordinateQuadrilateral = HCQ { _xtmin :: Double
                                             , _xtmax :: Double
                                             , _ytmin :: Double
                                             , _ytmax :: Double
                                             , _height :: Double }  deriving (Eq, Show)
-- instance Movable Obstacle where
--   tr !$ (HCQ _xtmin _xtmax _ytmin _ytmax _height) = Sphere (tr !$ p) r
  --tr !$ (Triangle q w e r) = Triangle (tr !$ q) (tr !$ w) (tr !$ e) r
   -- пока все будет твёрдое и со всеми видимыми рёбрами


   -- пока все будет твёрдое и со всеми видимыми рёбрами
--   старое положение    новое положение
-- pushOut :: (RealFloat a, Eq a, Ord a) => Obstacles a -> M44 a -> M44 a
-- pushOut a currentPos = foldr (\obstacle tr -> tr !*! pushOut obstacle
--                                                            ) currentPos a
--             where pushOut (Sphere center radius) = ((transposeMink (pushOutSphereO (fromV4 $ (toV4 center) *! currentPos ) radius )))
--                   pushOut (Triangle a b c r) = ((transposeMink (pushOutTriangleO (fromV4 $ (toV4 a) *! currentPos )
--                                                                                  (fromV4 $ (toV4 b) *! currentPos )
--                                                                                  (fromV4 $ (toV4 c) *! currentPos ) 
--                                                                                  r )))

decompose :: Point Double -> State -> State
decompose p (State pos height nod speed) = State (H.moveRightFromTo3 (pos !* (V3 0 0 1)) (projectToOxy p) !*! pos) (H.signedDistanceFromOxy p) nod 0
projectToOxy (Point q w e r) = V3 q w r
-- pushOut :: Obstacles Double -> State -> State
-- pushOut o s = foldr (\o1 -> pushOutOne o1) s o
--            where pushOutOne :: Obstacle Double -> State -> State
--                  pushOutOne (Sphere center radius) = if far center (_pos s)  then id else ((pushOutSphereO center radius ))
--                  pushOutOne (Triangle a b c r) = if far a (_pos s) then id else ((pushOutTriangleO a b c r))
--                  far (Point x y _ t) (m::M33 Double) = let (V3 xr yr tr) = m !* V3 0 0 1 
--                                          in abs ((x/t) - xr/tr)> 2 ||  abs ((y/t) - yr/tr)> 2
pushOut :: [RuntimeObstacle Double] -> State -> State
pushOut o s = foldr (\o1 -> pushOutOne o1) s o
           where pushOutOne :: RuntimeObstacle Double -> State -> State
                 pushOutOne (SphereR center radius) = if far center (_pos s)  then id else ((pushOutSphereO center radius ))
                 pushOutOne (TriangleR m x1 x2 y2 r) = {- far analysis could bw here -}(pushOutTriangleO m x1 x2 y2 r)
                 far (Point x y _ t) (m::M33 Double) = let (V3 xr yr tr) = m !* V3 0 0 1 
                                         in abs ((x/t) - xr/tr)> 2 ||  abs ((y/t) - yr/tr)> 2

-- pushOut :: Obstacles -> State -> State 
-- pushOut o s = foldr (\(OHCQ a) -> pushOutHorizontalCoordinateQuadrilateral a) s o

-- level = Env (Mesh [((0.0, 0.0, 1.0), (HE (Point 0.5 0.5 0 1) 
--                                          (Point 0.5 (-0.5) 0 1)
--                                          (Point (-0.5) 0.5 0 1))),
--                    ((1.0, 0.0,   0), (HE (Point (-0.5) (-sqrt(0.73)) 0.0 1) 
--                                          (Point 0.5 (-0.5) 0 1)
--                                          (Point (-0.5) 0.5 0 1)))])
--             [Triangle (Point 0.5 0.5 0 1) 
--                       (Point 0.5 (-0.5) 0 1)
--                       (Point (-0.5) 0.5 0 1) 0,
--             Triangle (Point (-0.5) (-sqrt(0.73)) 0.0 1) 
--                       (Point 0.5 (-0.5) 0 1)
--                       (Point (-0.5) 0.5 0 1) 0]--OHCQ (HCQ (-0.5) 0.5 (-0.5) 0.5 0), OHCQ (HCQ (-0.5) 0.5 (-0.5) 0.5 (-1))]

-- triangle = pushOutTriangleO (Point 0.5 0.5 0 1) 
--                             (Point 0.25 (-0.5) 0 1)
--                             (Point (-0.5) 0.25 0 1) 0

newtype Mesh color coordinate = Mesh [(color, HyperEntity coordinate)] deriving (Eq, Show,Functor, Read)
data HyperEntity a = Polygon [Point a] -- как всегда, для нормального отображения многоугольник должен быть выпуклым, и точки должны идти в порядке
                   | Segment !(Point a) !(Point a) 
                   | HPoint !(Point a) {- fixme this constructor isnt needed -} deriving (Eq, Show,Functor, Read)

instance H.Movable HyperEntity where
  a !$ (Polygon list) = Polygon $ map (a !$) list
  a !$ (Segment q w) = Segment (a !$ q) (a !$ w)

data Environment c a = Env { mesh :: !(Mesh c a),
                             obstacles :: !(Obstacles a) } deriving (Eq, Show, Read)
instance H.Movable (Environment c) where
  tr !$ Env (Mesh m) ob = Env (Mesh $ fmap (\(c, he) -> (c, tr !$ he)) m) (fmap (tr !$) ob)


data RuntimeObstacle a = SphereR !(Point a) a | TriangleR (M44 a) a a a a deriving (Eq, Show,Functor, Read)

-- pushOut :: (RealFloat a, Eq a, Ord a, Show a) => Obstacles a -> M44 a -> M44 a
-- pushOut (Obs a) currentPos = foldr (\(center, radius) a -> a !*! ((transposeMink (pushOutSphereO (fromV4 $ (toV4 center) *! currentPos ) radius )))
--                                                            ) currentPos a            


-- Тут мы много раз умножаем на identity, это можно оптимизировать разными 
-- способами, самый безболезненный, мне кажется - это добавить конструктор 
-- Identity в тип преобразований (M44)

-- \operatorname{ch}(x \pm y)=\operatorname{ch}x\,\operatorname{ch}y \pm \operatorname{sh}y\,\operatorname{sh}x.
--            центр сферы     радиус новое положение
-- pushOutOneOrigin :: Point Double -> Double -> M44 Double
-- pushOutOneOrigin m r = let 
--                         diff = r - distance origin m
--                        in  if (trace ("diff:" ++ show diff) diff) > 0 then moveTo m (-diff) else identityIm

-- pushOutOne :: Point Double -> Double -> Point Double -> M44 Double
-- pushOutOne center radius pos = {-commute (transposeMink $ moveRightTo pos) $-} pushOutOneOrigin (moveRightTo pos !$ center) radius
-- 1 - ((c+e-b-d)/(a - 2*b + c)) = (a - 2*b + c)/(a - 2*b + c) - ((c+e-b-d)/(a - 2*b + c)) = 
-- a - b + d - e     
pushOutHorizontalCoordinateQuadrilateral :: HorizontalCoordinateQuadrilateral -> State -> State
pushOutHorizontalCoordinateQuadrilateral (HCQ xtmin xtmax ytmin ytmax z) s@(State pos height nod _) 
   = let (V3 x y t) = pos !* (V3 0 0 1)
     in if abs (height + z ) <= ourSize && x/t > xtmin && x/t < xtmax && y/t > ytmin && y/t < ytmax
        then if (-height) < (z) then State pos ( ourSize - z) nod (V3 0 0 0) else  State pos ((-ourSize) - z) nod (V3 0 0 0) 
        else s-- podumay so znakami

computeObs :: Obstacles Double -> [RuntimeObstacle Double]
computeObs = map tr 
  where
    tr (Sphere q w) = SphereR q w
    tr (Triangle a b c r) 
     = TriangleR m x1 x2 y2 r
      where
        m = H.getTriangleToOxy a b c 
        V3 x1 _ _ = normalizePoint (H.toV4 $ m !$ b)
        V3 x2 y2 _ = normalizePoint (H.toV4 $ m !$ c) 
--                                 
pushOutSphereO :: Point Double -> Double -> State -> State
pushOutSphereO m r s = let 
                        diff = r - H.distance H.origin (currentPosition s)
                       in  if diff > 0 then decompose (H.moveFromTo m (currentPosition s) r !$m) s else s

trace :: String -> a -> a
trace s v = Debug.Trace.trace (s ++ show (Unsafe.Coerce.unsafeCoerce s::Double)) v
traceM :: String -> a -> a
traceM s v = Debug.Trace.trace (s ++ show (Unsafe.Coerce.unsafeCoerce s::M44 Double)) v
traceP :: String -> a -> a
traceP s v = Debug.Trace.trace (s ++ show (Unsafe.Coerce.unsafeCoerce s::Point Double)) v
-- nearestPoint :: (Floating a, Eq a, Ord a) => Point a -> Point a -> Point a -> Point a
-- nearestPoint a b c = toV4 

-- pushOutTriangleO :: Point Double -> Point Double -> Point Double -> Double -> State -> State
-- pushOutTriangleO !a !b !c !r !s = let 
--                             -- newb = getTriangleToOxy a b c !$ b
--                                 newO = m !$ currentPosition s
--                                 -- newc = getTriangleToOxy a b c !$ c
--                                 projOfNewO = let (Point x y z t) = newO in  (Point (x/t) (y/t) 0 1)
--                                 diff = r - distance newO projOfNewO 
--                                 m = getTriangleToOxy a b c 
--                                 notm = transposeMink m
--                                in
--                                 case normalizePoint (toV4 projOfNewO) of { V3 x y _ ->
--                                 case normalizePoint (toV4 $ m !$ b) of { V3 x1 _ _ ->
--                                 case normalizePoint (toV4 $ m !$ c) of { V3 x2 y2 _ ->
--                                 let
--                                   inside = (-y2*x) +(x2-x1)*y+x1*y2 > 0 && y > 0 && x/y > x2/y2
--                                 in
--                                  if inside && diff > (-ourSize) 
--                                   then decompose (moveFromTo (notm !$ projOfNewO) (notm !$ newO) (trace "diff: " r + ourSize) !$ (notm !$ projOfNewO)) s 
--                                   else s
--                                   } } }

pushOutTriangleO :: M44 Double -> Double -> Double -> Double -> Double -> State -> State
pushOutTriangleO !m !x1 !x2 !y2 !r !s = let 
                            -- newb = getTriangleToOxy a b c !$ b
                                newO = m !$ currentPosition s
                                -- newc = getTriangleToOxy a b c !$ c
                                projOfNewO = let (Point x y _ t) = newO in  (Point (x) (y) 0 t)
                                diff = r - H.distance newO projOfNewO 
                                notm = H.transposeMink m
                               in
                                case normalizePoint (H.toV4 projOfNewO) of { V3 x y _ ->
                                let
                                  inside = (-y2*x) +(x2-x1)*y+x1*y2 > 0 && y > 0 && x/y > x2/y2
                                in
                                 if inside && diff > ((-ourSize) +0.00001)
                                  then decompose (H.moveFromTo (notm !$ projOfNewO) (notm !$ newO) (r + ourSize) !$ (notm !$ projOfNewO)) s 
                                  else s
                                  } 


--debug :: HasCallStack
-- debug = pushOutTriangleO  (moveAlongX 0.3 !$ (Point 0 (-sinh 3) (-sinh 3) 14.202662994046431)) 
--                           (moveAlongX 0.3 !$ (Point 0 (sinh 3) (-sinh 3) 14.202662994046431)) 
--                           (moveAlongX 0.3 !$ Point 0 0 (sinh 3) (cosh 3)) 
--                           3

-- pushOutTriangle :: (Floating a, Eq a, Ord a, Show a) => Point a -> Point a -> Point a -> Point a -> M44 a
-- pushOutTriangle b k r p = 
--     let dp = b ^-^ p
--         e0 = k ^-^ b
--         e1 = r ^-^ b
--         a = form e0 e0
--         b = form e0 e1 
--         c = form e1 e1
--         d = forn e0 dp 
--         e = form e1 dp
--         f = form dp dp
--         det = a * c - b * b 
--         s = b * e - c * d 
--         t = b * d - a * e

--         region0 = quad (s/det) (t/det)
--         region1 = if (c+e-b-d) <= 0 
--                   then quad 0 1
--                   else if (c+e-b-d) >= a - 2*b + c 
--                        then quad 1 0
--                        else quad ((c+e-b-d)/(a - 2*b + c)) ((a - b + d - e)/(a - 2*b + c)) 
--                        -- тут можно устроить common subexpr elimination, потому что сумма этих аргументов равна 1
--         region3 = if e >= 0 
--                   then quad 0 0
--                   else if (-e) >= c 
--                        then quad 0 1
--                        else quad 0 ((-e)/c)
--         region5 = if d >= 0 
--                   then quad 0 0
--                   else if (-d) >= a 
--                        then quad 0 1
--                        else quad 0 ((-d)/a)
--         region2 = if 

--     in if (s+t <= det) 
--        then if (s < 0) 
--             then if t < 0
--                  then region4
--                  else region3
--             else if t < 0 
--                  then region5 
--                  else region0
--        else if s < 0
--             then region2
--             else if t < 0 
--                  then region6
--                  else region1





{-
https://www.geometrictools.com/Documentation/DistancePoint3Triangle3.pdf

Что такое треугольник?
Это выпуклая оболочка трёх точек.
Пусть есть три точки B, K, R  (их координаты = Bx, By, Kx и т д.
Обозначим E0 = K ^-^ B, E1 = R ^-^ B. Эти векторы зависят от конкретного 
представления точек. Например, если B = 0 0 (sinh 1) (cosh 1), K = 0 0 0 1,
то E0 = 0 0 (-sinh 1) (1-cosh 1). K - B/(cosh 1) = 0 0 (-tanh 1) 0. 
Есть множество точек B + s * E0 + t * E1. Надо доказать, что это множество и будет нашим треугольником.
Это множество ограничено прямыми. Будем пока считать, что этого достаточно.

Интуиция говорит, что трансцендентную операцию можно сделать один раз.
Нам достаточно обрабатывать только нулевой регион (наверное).
Для этого нам надо посчитать расстояние до плоскости и установить, что мы проецируемся внутрь треугольника.
Надо тупо найти проекцию нас на треугольник.

мы в точке p.
Треугольник A B C.
Надо найти точку D линейно зависимую с A B C такую что отрезок p-D перпендикулярен плоскости ABC. 
Когда отрезок перпендикулярен плоскости, задаваемой тремя точками?

Когда два отрезка перпендикулярны? Когда они переводятся друг в друга поворотом на tau/4? Когда их можно перевести в Ox, Oy?
Мы должны перевести эту точку в Oz и эту плоскость в Oxy. Чтобы перевести плоскость в Oxy, надо найти её пересечение с Oxy и поернуть вогруг него на
сколько надо. Пересечение может быть в идеальной области. 

Это всё неправильно. Надо повернуть вокруг Oz точку p в плоскость OXZ, потом повернуть вокруг OY в ось Oz, подвинуть вдоль Oz в начало координат. А потом?.....

Это всё неправильно. Надо перевести точку A в начало координат. Потом перевести точку B в Ox. Потом пепевести точку D в Oxy. Тогда проектировать надо на Oxy.

Как проецировать на Oxy? 
В нормальных координатах триdиально. В координатах проекции клейна тоже тривиально. т е координаты проекции точки x y z t  -  x/t y/t 0 1 = x y 0 t (ВНЕЗАПНО)


-}

