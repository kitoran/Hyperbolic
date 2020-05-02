#ifndef PHYSICS_H
#define PHYSICS_H
#include <vector>
#include <algorithm>
#include <numeric>
#include "hyperbolic.h"
#include "serialize.h"
#include <boost/optional.hpp>

//module Physics where

//import qualified Hyperbolic as H
//import qualified Unsafe.Coerce
//-- import Data.Foldable
//import qualified Debug.Trace
//import Data.MonoTraversable
//-- import qualified GHC.Exts
//import qualified Control.Lens as Lens
//import Data.IORef
//import Linear(M44, (!*!), (!*), (*!), normalizePoint, V3(..), M33)

//--fixme этот файл, конечно, надо переместить в util, а Mesh переместить в другой файл

//--текущее положение  матрица куда надо пойти   результат (?)
//-- correct :: M44 Double -> M44 Double -> M44 Double
//-- correct
struct Source {
    H::Point p;
    H::Absolute a;
};
inline Source operator *(const H::Matrix44& m, const Source& d) {
    return {m*d.p, m*d.a};
}
inline void serialize(FILE* stream, const Source& o) {
    serialize(stream, o.p);
    serialize(stream, o.a);
}
inline void deserialize(FILE* stream, Source* o) {
    deserialize(stream, &o->p);
    deserialize(stream, &o->a);
}
using Receiver = std::vector<H::Point>;
//type instance Element Receiver = H.Point Double
enum ObstacleType { Sphere , Triangle };
struct Obstacle {
    ObstacleType type;
    union {
        struct {
            H::Point center;
            double radius;
        };
        struct {
            H::Point a;
            H::Point b;
            H::Point c;
            double thickness;
        };
    };    
};
inline Obstacle operator *(const H::Matrix44& m, const Obstacle& d) {
    if(d.type != Triangle) {
        std::terminate();
    }
    Obstacle res = d;
    res.a = m*res.a;
    res.b = m*res.b;
    res.c = m*res.c;
    return res;
}
using Obstacles = std::vector<Obstacle>;
enum HyperEntityType { Polygon, Segment, HPoint};
struct HyperEntity {
    HyperEntityType type;
    std::vector<H::Point> p; //-- как всегда, для нормального отображения многоугольник должен быть выпуклым, и точки должны идти в порядке
};
inline void serialize(FILE* stream, const HyperEntity& o) {
    serialize(stream, o.type);
    serialize(stream, o.p);
}
inline void deserialize(FILE* stream, HyperEntity* o) {
    deserialize(stream, &o->type);
    deserialize(stream, &o->p);
}
using namespace H;
inline HyperEntity operator *(H::Matrix44 m, HyperEntity a) {
    std::vector<H::Point> e;
    e.reserve(a.p.size());
    std::transform(a.p.begin(), a.p.end(), std::back_inserter(e), [&](Point ce) -> Point {
//        ColoredEntity r;
//        r.color = ce.color;
//        r.e = m*
        return m*ce;
    });

    return {a.type, e};
}
union  Color {
    struct {
        float r;
        float g;
        float b;
        float a;
    };
    float m[4];
};
inline void serialize(FILE* stream, const Color& o) {
    fwrite(o.m, sizeof(o.m[0]), 4, stream);
}
inline void deserialize(FILE* stream, Color* o) {
    if(fread(o->m, sizeof(o->m[0]), 4, stream) != 4) {
        throw DeserializeException();
    }
}
struct ColoredEntity {
    Color color;
    HyperEntity e;
};
inline void serialize(FILE* stream, const ColoredEntity& o) {
    serialize(stream, o.color);
    serialize(stream, o.e);
}
inline void deserialize(FILE* stream, ColoredEntity* o) {
    deserialize(stream, &o->color);
    deserialize(stream, &o->e);
}
using Mesh = std::vector<ColoredEntity>;// [((Double, Double, Double, Double), HyperEntity)] deriving ( Show, Read, Monoid, MonoFunctor, Ord, Eq)

inline Mesh operator *(H::Matrix44 m, Mesh a) {
    Mesh r;
    r.reserve(a.size());
    std::transform(a.begin(), a.end(), std::back_inserter(r), [&](ColoredEntity ce) -> ColoredEntity {
//        ColoredEntity r;
//        r.color = ce.color;
//        r.e = m*
        return {ce.color, m*ce.e};
    });

    return r;
}
template <typename T>
inline std::vector<T> operator *(const H::Matrix44& m, const std::vector<T>& d) {
    std::vector<T> r; r.reserve(d.size());
    for(const auto& w : d) {
        r.push_back(m*w);
    }
    return r;
}
//type instance Element Mesh = ((Double, Double, Double, Double), HyperEntity)
struct Environment {
    Mesh mesh;
    Obstacles obstacles;
    std::vector<Source> sources;
    std::vector<Receiver> receivers;
    Environment& operator +=(const Environment& d) {
        mesh.insert(mesh.end(), d.mesh.begin(), d.mesh.end());
        obstacles.insert(obstacles.end(), d.obstacles.begin(), d.obstacles.end());
        sources.insert(sources.end(), d.sources.begin(), d.sources.end());
        receivers.insert(receivers.end(), d.receivers.begin(), d.receivers.end());
        return *this;
    }
};
inline Environment operator *(const H::Matrix44& m, const Environment& d) {
    return {m*d.mesh, m*d.obstacles, m*d.sources, m*d.receivers};
}
//$(Lens.makeLenses ''Environment)
struct AvatarPosition  {
    Matrix33 pos; // проекция на плоскость z=0
    double height;
    double nod;
    Vector3 speed;
};
AvatarPosition toAvatarPosition(const Matrix44& st);
Matrix44 fromAvatarPosition(const AvatarPosition &ap);
enum Item { Empty, De, Di};
struct Deviator {
    H::Point pos;
    Absolute dir;
    double nod;
}; // отклоняет поток на 90 градусов
inline Deviator operator *(const H::Matrix44& m, const Deviator& d) {
    return {m*d.pos, d.dir.move(m), d.nod}; // третий аргумет переводится неправильно, но что поделать
}
struct Divider {
    H::Point pos;
    Absolute dir;
    double nod;
};
struct WorldState {
    std::vector<Deviator> devis;
    std::vector<Divider> divis;
};
using OptionalInt = boost::optional<int>;
using OptionalDouble = boost::optional<double>;
struct LevelState {
    AvatarPosition avatarPosition;
    Item inventory;
    WorldState worldState;
    OptionalInt selected;
};

//instance (Monoid t, H.Movable t (H.Point Double), H.Movable t (Absolute Double)) => H.Movable t (Deviator ) where
//  trans !$ (Devi a b c) = Devi (trans !$ a) (trans !$ b) c -- третий аргумет переводится неправильно, но что поделать





//$(Lens.makeLenses ''AvatarPosition)
//$(Lens.makeLenses ''LevelState)

//currentPosition (AP (!pos) (!height) _ _) =  H.m33_to_m44M pos !*! H.moveAlongZ height !$ H.origin
inline Point currentPosition(const AvatarPosition &ap) {
    return H::m33_to_m44M(ap. pos) * (H::moveAlongZ( ap.height) * H::origin);
}
static auto ourSize = 0.1;

//instance (Monoid t, H.Movable t (Point Double)) => H.Movable t Obstacle where
//  (!tr) !$ (Sphere !p !r) = Sphere (tr !$ p) r
//  (!tr) !$ (Triangle !q !w !e !r) = Triangle (tr !$ q) (tr !$ w) (tr !$ e) r

//domainRadius :: Double
//domainRadius = acosh (cosh al * cosh bl)
//  where a, b, c, al, bl :: Double
//        a = tau/10
//        b = tau/10
//        c = tau/4
//        al = acosh ( cos a / sin b )
//        bl = acosh ( cos b / sin a )

//-- breakDown :: Obstacles Double ->




//-- data Obstacle = OHCQ HorizontalCoordinateQuadrilateral deriving (Eq, Show)
//data HorizontalCoordinateQuadrilateral = HCQ { _xtmin :: Double
//                                             , _xtmax :: Double
//                                             , _ytmin :: Double
//                                             , _ytmax :: Double
//                                             , _height :: Double }  deriving (Eq, Show)
//-- instance Movable Obstacle where
//--   tr !$ (HCQ _xtmin _xtmax _ytmin _ytmax _height) = Sphere (tr !$ p) r
//  --tr !$ (Triangle q w e r) = Triangle (tr !$ q) (tr !$ w) (tr !$ e) r
//   -- пока все будет твёрдое и со всеми видимыми рёбрами


//   -- пока все будет твёрдое и со всеми видимыми рёбрами
//--   старое положение    новое положение
//-- pushOut :: (RealFloat a, Eq a, Ord a) => Obstacles a -> M44 a -> M44 a
//-- pushOut a currentPos = foldr (\obstacle tr -> tr !*! pushOut obstacle
//--                                                            ) currentPos a
//--             where pushOut (Sphere center radius) = ((transposeMink (pushOutSphereO (fromV4 $ (toV4 center) *! currentPos ) radius )))
//--                   pushOut (Triangle a b c r) = ((transposeMink (pushOutTriangleO (fromV4 $ (toV4 a) *! currentPos )
//--                                                                                  (fromV4 $ (toV4 b) *! currentPos )
//--                                                                                  (fromV4 $ (toV4 c) *! currentPos )
//--                                                                                  r )))

//decompose :: Point Double -> AvatarPosition -> AvatarPosition
//decompose p (AP pos height nod speed) = AP (H.moveRightFromTo3 (pos !* (V3 0 0 1)) (projectToOxy p) !*! pos) (H.signedDistanceFromOxy p) nod 0
inline Vector3 projectToOxy (const Point & p)  {
    return {p.x, p.y, p.t};
}
inline auto decompose (Point p, const AvatarPosition &s ) -> AvatarPosition {
    return {H::moveRightFromTo3(s.pos * Vector3{0,0,1}, projectToOxy (p)) * s.pos, H::signedDistanceFromOxy (p), s.nod, {0,0,0}};
}
//-- pushOut :: Obstacles Double -> State -> State
//-- pushOut o s = foldr (\o1 -> pushOutOne o1) s o
//--            where pushOutOne :: Obstacle Double -> State -> State
//--                  pushOutOne (Sphere center radius) = if far center (_pos s)  then id else ((pushOutSphereO center radius ))
//--                  pushOutOne (Triangle a b c r) = if far a (_pos s) then id else ((pushOutTriangleO a b c r))
//--                  far (Point x y _ t) (m::M33 Double) = let (V3 xr yr tr) = m !* V3 0 0 1
//--                                          in abs ((x/t) - xr/tr)> 2 ||  abs ((y/t) - yr/tr)> 2
//pushOut :: [RuntimeObstacle ] -> AvatarPosition -> AvatarPosition
//pushOut o s = foldr (\o1 -> pushOutOne o1) s o
//           where pushOutOne :: RuntimeObstacle -> AvatarPosition -> AvatarPosition
//                 pushOutOne (SphereR center radius) = if far center ((_pos::AvatarPosition -> M33 Double) s)  then id else ((pushOutSphereO center radius ))
//                 pushOutOne (TriangleR m x1 x2 y2 r) = {- far analysis could bw here -}(pushOutTriangleO m x1 x2 y2 r)
//                 far (Point x y _ t) (m::M33 Double) = let (V3 xr yr tr) = m !* V3 0 0 1
//                                         in abs ((x/t) - xr/tr)> 2 ||  abs ((y/t) - yr/tr)> 2
enum RuntimeObstacleType { SphereR, TriangleR };
struct RuntimeObstacle {
    RuntimeObstacleType  type;
    union {
        struct {
            Point center;
            double radius;
        };
        struct {
            Matrix44 trans;
            double x1;
            double x2;
            double y2;
            double thickness;
        };
    };
};
AvatarPosition pushOutSphereO(Point m, double r, const AvatarPosition&s);
AvatarPosition pushOutTriangleO(const Matrix44& m, double x1, double x2, double y2, double r, const AvatarPosition& s);

inline AvatarPosition pushOut(const std::vector<RuntimeObstacle> o, const AvatarPosition& s) {
//    far (Point x y _ t) (m::M33 Double) = let (V3 xr yr tr) = m !* V3 0 0 1
//            in abs ((x/t) - xr/tr)> 2 ||  abs ((y/t) - yr/tr)> 2
    auto pushOutOne = [](const AvatarPosition& p, const RuntimeObstacle& o) -> AvatarPosition {
        if(o.type == SphereR) {
          /*if far (center, p.pos)  {
              return p;
          } else */return (pushOutSphereO (o.center,  o.radius, p ));
        }
        if(o.type == TriangleR) {
            /*{- far analysis could bw here -}*/return pushOutTriangleO (o.trans, o.x1, o.x2,  o.y2, o.thickness, p);
        }
        std::terminate();
    };
    return std::accumulate(o.begin(), o.end(), s, pushOutOne);
}
//-- pushOut :: Obstacles -> State -> State
//-- pushOut o s = foldr (\(OHCQ a) -> pushOutHorizontalCoordinateQuadrilateral a) s o

//-- level = Env (Mesh [((0.0, 0.0, 1.0), (HE (Point 0.5 0.5 0 1)
//--                                          (Point 0.5 (-0.5) 0 1)
//--                                          (Point (-0.5) 0.5 0 1))),
//--                    ((1.0, 0.0,   0), (HE (Point (-0.5) (-sqrt(0.73)) 0.0 1)
//--                                          (Point 0.5 (-0.5) 0 1)
//--                                          (Point (-0.5) 0.5 0 1)))])
//--             [Triangle (Point 0.5 0.5 0 1)
//--                       (Point 0.5 (-0.5) 0 1)
//--                       (Point (-0.5) 0.5 0 1) 0,
//--             Triangle (Point (-0.5) (-sqrt(0.73)) 0.0 1)
//--                       (Point 0.5 (-0.5) 0 1)
//--                       (Point (-0.5) 0.5 0 1) 0]--OHCQ (HCQ (-0.5) 0.5 (-0.5) 0.5 0), OHCQ (HCQ (-0.5) 0.5 (-0.5) 0.5 (-1))]

//-- triangle = pushOutTriangleO (Point 0.5 0.5 0 1)
//--                             (Point 0.25 (-0.5) 0 1)
//--                             (Point (-0.5) 0.25 0 1) 0



//instance (Monoid t, H.Movable t (Point Double)) => H.Movable t HyperEntity where
//  a !$ (Polygon list) = Polygon $ map (a !$) list
//  a !$ (Segment q w) = Segment (a !$ q) (a !$ w)

//instance (Monoid t, H.Movable t (Point Double)) => H.Movable t Mesh where
//  a !$ (Mesh l) = Mesh $ fmap (\(c, he) -> (c, a !$ he)) l

//instance (Monoid t, H.Movable t (Point Double),  H.Movable t (H.Absolute Double)) => H.Movable t (Environment) where
//  tr !$ Env m ob s a = Env (tr !$ m) (fmap (tr !$) ob) (fmap (tr !$) s) (fmap (tr !$) a)

//instance (Monoid t, H.Movable t (H.Point Double), H.Movable t (H.Absolute Double)) => H.Movable t (Source) where
//  tr !$ (Source p a) = Source (tr !$ p) (tr !$ a)
//instance (Monoid t, H.Movable t (H.Point Double), H.Movable t (H.Absolute Double)) => H.Movable t (Receiver) where
//  (!$) m = omap $ (!$) m


//-- -- optics
//-- ray :: V3 Double -> Double -> S.Set Mirror -> ([V3 Double], Double)
//-- ray pp phi s = case findFirstIntersection pp phi s of
//--           Nothing -> ([], phi)
//--           Just (p, d) -> (p:fst (ray p d s), d)
//-- findFirstIntersection :: V3 Double -> Double -> S.Set Mirror -> Maybe (V3 Double, Double)
//-- findFirstIntersection p phi s = foldrM
//--                                   (Just . min)
//--                                   (map (\m -> intersection p phi m) s)
//--                                   Nothing
//-- intersection :: V3 Double -> Double -> Mirror -> Maybe Double
//-- intersection p phi (p1, p2) = let ((V3 x1 y1 z1), (V3 x2 y2 z2)) = (rotate2 (-phi) !*! moveToOrigin2 p !* ) *** (p1, p2)
//--                                   res = (x2*z1-x1*z2)/(y1*z2-y2*z1)
//--                               in if y1*z1*y1*z2 < 0 && res > 0 then Just res else Nothing
//-- pushOut :: (RealFloat a, Eq a, Ord a, Show a) => Obstacles a -> M44 a -> M44 a
//-- pushOut (Obs a) currentPos = foldr (\(center, radius) a -> a !*! ((transposeMink (pushOutSphereO (fromV4 $ (toV4 center) *! currentPos ) radius )))
//--                                                            ) currentPos a


//-- Тут мы много раз умножаем на identity, это можно оптимизировать разными
//-- способами, самый безболезненный, мне кажется - это добавить конструктор
//-- Identity в тип преобразований (M44)

//-- \operatorname{ch}(x \pm y)=\operatorname{ch}x\,\operatorname{ch}y \pm \operatorname{sh}y\,\operatorname{sh}x.
//--            центр сферы     радиус новое положение
//-- pushOutOneOrigin :: Point Double -> Double -> M44 Double
//-- pushOutOneOrigin m r = let
//--                         diff = r - distance origin m
//--                        in  if (trace ("diff:" ++ show diff) diff) > 0 then moveTo m (-diff) else identityIm

//-- pushOutOne :: Point Double -> Double -> Point Double -> M44 Double
//-- pushOutOne center radius pos = {-commute (transposeMink $ moveRightTo pos) $-} pushOutOneOrigin (moveRightTo pos !$ center) radius
//-- 1 - ((c+e-b-d)/(a - 2*b + c)) = (a - 2*b + c)/(a - 2*b + c) - ((c+e-b-d)/(a - 2*b + c)) =
//-- a - b + d - e
//pushOutHorizontalCoordinateQuadrilateral :: HorizontalCoordinateQuadrilateral -> AvatarPosition -> AvatarPosition
//pushOutHorizontalCoordinateQuadrilateral (HCQ xtmin xtmax ytmin ytmax z) s@(AP pos height nod _)
//   = let (V3 x y t) = pos !* (V3 0 0 1)
//     in if abs (height + z ) <= ourSize && x/t > xtmin && x/t < xtmax && y/t > ytmin && y/t < ytmax
//        then if (-height) < (z) then AP pos ( ourSize - z) nod (V3 0 0 0) else  AP pos ((-ourSize) - z) nod (V3 0 0 0)
//        else s-- podumay so znakami

//computeObs :: Obstacles  -> [RuntimeObstacle ]
//computeObs = map tr
//  where
//    tr (Sphere q w) = SphereR q w
//    tr (Triangle a b c r)
//     = TriangleR m x1 x2 y2 r
//      where
//        m = H.getTriangleToOxy a b c
//        V3 x1 _ _ = normalizePoint (H.toV4 $ m !$ b)
//        V3 x2 y2 _ = normalizePoint (H.toV4 $ m !$ c)
inline std::vector<RuntimeObstacle> computeObs( const Obstacles & o) {
  auto tr = [](const Obstacle& o) -> RuntimeObstacle  {
      if(o.type == Sphere) {
         return {SphereR,  {o.center, o.radius}};
      }
      if(o.type == Triangle) {
          Matrix44 m = H::getTriangleToOxy(o.a, o.b, o.c);
          double x1 = klein((m * o.b)).x;
//          V3 x2 y2 _ =
          auto rere = klein(m * o.c);
          RuntimeObstacle res;
          res.type = TriangleR;
          res.trans = m;
          res.x1 = x1;
          res.x2 = rere.x;
          res.y2 = rere.y;
          res.thickness = o.thickness;
          return res;
      }
      std::terminate();
  };
    std::vector<RuntimeObstacle> res;
  res.reserve(o.size());
    for(const Obstacle& no : o) {
        res.push_back(tr(no));
    }
    return res;
}
//--
inline AvatarPosition pushOutSphereO(Point m, double r, const AvatarPosition&s) {
  auto diff = r - H::distance( H::origin,  (currentPosition (s)));
  if (diff > 0) {
      return decompose (H::moveFromTo (m, (currentPosition (s)), r) * m, s);
  } else  return s;
}
//trace :: String -> a -> a
//trace s v = Debug.Trace.trace (s ++ show (Unsafe.Coerce.unsafeCoerce s::Double)) v
//traceM :: String -> a -> a
//traceM s v = Debug.Trace.trace (s ++ show (Unsafe.Coerce.unsafeCoerce s::M44 Double)) v
//traceP :: String -> a -> a
//traceP s v = Debug.Trace.trace (s ++ show (Unsafe.Coerce.unsafeCoerce s::Point Double)) v
//-- nearestPoint :: (Floating a, Eq a, Ord a) => Point a -> Point a -> Point a -> Point a
//-- nearestPoint a b c = toV4

//-- pushOutTriangleO :: Point Double -> Point Double -> Point Double -> Double -> State -> State
//-- pushOutTriangleO !a !b !c !r !s = let
//--                             -- newb = getTriangleToOxy a b c !$ b
//--                                 newO = m !$ currentPosition s
//--                                 -- newc = getTriangleToOxy a b c !$ c
//--                                 projOfNewO = let (Point x y z t) = newO in  (Point (x/t) (y/t) 0 1)
//--                                 diff = r - distance newO projOfNewO
//--                                 m = getTriangleToOxy a b c
//--                                 notm = transposeMink m
//--                                in
//--                                 case normalizePoint (toV4 projOfNewO) of { V3 x y _ ->
//--                                 case normalizePoint (toV4 $ m !$ b) of { V3 x1 _ _ ->
//--                                 case normalizePoint (toV4 $ m !$ c) of { V3 x2 y2 _ ->
//--                                 let
//--                                   inside = (-y2*x) +(x2-x1)*y+x1*y2 > 0 && y > 0 && x/y > x2/y2
//--                                 in
//--                                  if inside && diff > (-ourSize)
//--                                   then decompose (moveFromTo (notm !$ projOfNewO) (notm !$ newO) (trace "diff: " r + ourSize) !$ (notm !$ projOfNewO)) s
//--                                   else s
//--                                   } } }

//pushOutTriangleO :: M44 Double -> Double -> Double -> Double -> Double -> AvatarPosition -> AvatarPosition
//pushOutTriangleO !m !x1 !x2 !y2 !r !s = let
//                            -- newb = getTriangleToOxy a b c !$ b
//                                newO = m !$ currentPosition s
//                                -- newc = getTriangleToOxy a b c !$ c
//                                projOfNewO = let (Point x y _ t) = newO in  (Point (x) (y) 0 t)
//                                diff = r - H.distance newO projOfNewO
//                                notm = H.transposeMink m
//                               in
//                                case normalizePoint (H.toV4 projOfNewO) of { V3 x y _ ->
//                                let
//                                  inside = (-y2*x) +(x2-x1)*y+x1*y2 > 0 && y > 0 && x/y > x2/y2
//                                in
//                                 if inside && diff > ((-ourSize) +0.00001)
//                                  then decompose (H.moveFromTo (notm !$ projOfNewO) (notm !$ newO) (r + ourSize) !$ (notm !$ projOfNewO)) s
//                                  else s
//                                  }
inline auto pushOutTriangleO(const Matrix44& m, double x1, double x2, double y2, double r, const AvatarPosition& s) -> AvatarPosition {
//                            -- newb = getTriangleToOxy a b c !$ b
    auto newO = m * currentPosition( s);
//                                -- newc = getTriangleToOxy a b c !$ c
    auto projOfNewO = newO; projOfNewO.z = 0;
    auto diff = r - H::distance(newO, projOfNewO);
    auto notm = H::transposeMink(m);
    auto d = klein(projOfNewO);

    auto inside = (-y2*d.x) +(x2-x1)*d.y+x1*y2 > 0 && d.y > 0 && d.x/d.y > x2/y2;
    if( inside && diff > ((-ourSize) +0.00001)) {
        return decompose (H::moveFromTo (notm * projOfNewO, notm * newO, r + ourSize) * (notm * projOfNewO), s);
    } else return s;
}

//--debug :: HasCallStack
//-- debug = pushOutTriangleO  (moveAlongX 0.3 !$ (Point 0 (-sinh 3) (-sinh 3) 14.202662994046431))
//--                           (moveAlongX 0.3 !$ (Point 0 (sinh 3) (-sinh 3) 14.202662994046431))
//--                           (moveAlongX 0.3 !$ Point 0 0 (sinh 3) (cosh 3))
//--                           3

//-- pushOutTriangle :: (Floating a, Eq a, Ord a, Show a) => Point a -> Point a -> Point a -> Point a -> M44 a
//-- pushOutTriangle b k r p =
//--     let dp = b ^-^ p
//--         e0 = k ^-^ b
//--         e1 = r ^-^ b
//--         a = form e0 e0
//--         b = form e0 e1
//--         c = form e1 e1
//--         d = forn e0 dp
//--         e = form e1 dp
//--         f = form dp dp
//--         det = a * c - b * b
//--         s = b * e - c * d
//--         t = b * d - a * e

//--         region0 = quad (s/det) (t/det)
//--         region1 = if (c+e-b-d) <= 0
//--                   then quad 0 1
//--                   else if (c+e-b-d) >= a - 2*b + c
//--                        then quad 1 0
//--                        else quad ((c+e-b-d)/(a - 2*b + c)) ((a - b + d - e)/(a - 2*b + c))
//--                        -- тут можно устроить common subexpr elimination, потому что сумма этих аргументов равна 1
//--         region3 = if e >= 0
//--                   then quad 0 0
//--                   else if (-e) >= c
//--                        then quad 0 1
//--                        else quad 0 ((-e)/c)
//--         region5 = if d >= 0
//--                   then quad 0 0
//--                   else if (-d) >= a
//--                        then quad 0 1
//--                        else quad 0 ((-d)/a)
//--         region2 = if

//--     in if (s+t <= det)
//--        then if (s < 0)
//--             then if t < 0
//--                  then region4
//--                  else region3
//--             else if t < 0
//--                  then region5
//--                  else region0
//--        else if s < 0
//--             then region2
//--             else if t < 0
//--                  then region6
//--                  else region1





//{-
//https://www.geometrictools.com/Documentation/DistancePoint3Triangle3.pdf

//Что такое треугольник?
//Это выпуклая оболочка трёх точек.
//Пусть есть три точки B, K, R  (их координаты = Bx, By, Kx и т д.
//Обозначим E0 = K ^-^ B, E1 = R ^-^ B. Эти векторы зависят от конкретного
//представления точек. Например, если B = 0 0 (sinh 1) (cosh 1), K = 0 0 0 1,
//то E0 = 0 0 (-sinh 1) (1-cosh 1). K - B/(cosh 1) = 0 0 (-tanh 1) 0.
//Есть множество точек B + s * E0 + t * E1. Надо доказать, что это множество и будет нашим треугольником.
//Это множество ограничено прямыми. Будем пока считать, что этого достаточно.

//Интуиция говорит, что трансцендентную операцию можно сделать один раз.
//Нам достаточно обрабатывать только нулевой регион (наверное).
//Для этого нам надо посчитать расстояние до плоскости и установить, что мы проецируемся внутрь треугольника.
//Надо тупо найти проекцию нас на треугольник.

//мы в точке p.
//Треугольник A B C.
//Надо найти точку D линейно зависимую с A B C такую что отрезок p-D перпендикулярен плоскости ABC.
//Когда отрезок перпендикулярен плоскости, задаваемой тремя точками?

//Когда два отрезка перпендикулярны? Когда они переводятся друг в друга поворотом на tau/4? Когда их можно перевести в Ox, Oy?
//Мы должны перевести эту точку в Oz и эту плоскость в Oxy. Чтобы перевести плоскость в Oxy, надо найти её пересечение с Oxy и поернуть вогруг него на
//сколько надо. Пересечение может быть в идеальной области.

//Это всё неправильно. Надо повернуть вокруг Oz точку p в плоскость OXZ, потом повернуть вокруг OY в ось Oz, подвинуть вдоль Oz в начало координат. А потом?.....

//Это всё неправильно. Надо перевести точку A в начало координат. Потом перевести точку B в Ox. Потом пепевести точку D в Oxy. Тогда проектировать надо на Oxy.

//Как проецировать на Oxy?
//В нормальных координатах триdиально. В координатах проекции клейна тоже тривиально. т е координаты проекции точки x y z t  -  x/t y/t 0 1 = x y 0 t (ВНЕЗАПНО)


//-}



#endif // PHYSICS_H

