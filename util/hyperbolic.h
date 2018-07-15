#ifndef HYPERBOLIC_H
#define HYPERBOLIC_H
#include <initializer_list>
#include "util/linear.h"
#include "math.h"
#define FOR4(name) for(int name : {0, 1, 2, 3})
#define FOR3(name) for(int name : {0, 1, 2})
#define FOR16(name) for(int name = 0; name < 16; name++)
namespace H {

using Component = double;
union Point {
    struct {
        Component x;
        Component y;
        Component z;
        Component t;
    };
    Component data[4];
    Component & operator[](int a);
    Point operator-() { // Это не противоположная точка, это та же самая точка
        return {{-x, -y, -z, -t}};
    }
    bool operator<(const Point&o) const {
        return x+y+z+t < o.x + o.y + o.z + o.t;
    }
/// for proper point x^2 + y^2 + z^2 - t^2 < 0 so this map
///  is not nearly injective, that's sad. However, sometimes i use improper points
};
union Vector3 {
    struct {
        Component x;
        Component y;
        union {
            Component t;
            Component z;
        };
    };
    Component data[3];
    Component & operator[](int a);
    double norm();
    Vector3 operator-(Vector3 o) {
        return {{x-o.x, y-o.y, z-o.z}};
    }
};
Vector3 cross(Vector3 p, Vector3 r);
Vector3 fmap(auto f, Vector3 t) {
    Vector3 r;
    FOR3(j) {
        r[j] = f(t[j]);
    }
    return r;
}
double dot(Vector3 a, Vector3 b);
union Vector2 {
    struct {
        Component x;
        Component y;

    };
    Component data[2];
    Component & operator[](int a);
    double norm();
    Component operator* (const Vector2& o) {
        return x*o.x + y*o.y;
    }
    Vector2& operator *=(Component a) {
      x *= a;
      y *= a;
      return *this;
    }
};
Component pseudoscalar(Vector2 p, Vector2 r);
struct Matrix22 {
    Component m[4];
    Vector2 operator*(const Vector2& o) {
        return {m[0]*o.x + m[1]*o.y, m[2]*o.x + m[3]*o.y};
    }
};
Matrix22 inv22 (const Matrix22& o);
struct Matrix44 {
    Component m[16];
    Component& operator()(int i, int j);
    Component operator()(int i, int j) const;
//    Matrix44& operator *=(Matrix44 o);
    Matrix44 operator /(double a) {
        Matrix44 r;
        FOR16(i) {
            r.m[i] = m[i]/a;
        }
        return r;
    }
};
Matrix44 inv44(Matrix44 a);
struct Matrix33 {
    Component m[9];
    Component& operator()(int i, int j);
    Component operator()(int i, int j) const;
};
Matrix44 transpose(Matrix44 a);
Matrix33 transpose3(Matrix33 a);

/*
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
констрейнта (Floating  a) в большинство функций and overall inefficient.

Уровни абстракции расположены так:
                4-мерное пространство
                /                   \
              |/                     \|
               --                   --
Пространство Минковского    Проективная плоскость
               \                       /
                \|                   |/
               --                     --
*Пространство Лобачевского (с идеальными элементами)

В правой системе координат если Ox направлена  вперёд, а Oz вверх, то Oy направлена влево!
-}
*/
inline double sign(double a) {
    return copysign(1.0, a);
}
Vector3 klein(Point p);

constexpr double tau = 6.2831853071795864769252867665590057683943;
Matrix44 operator *(const Matrix44 a, const Matrix44 b);
Matrix33 operator *(const Matrix33 a, const Matrix33 b);
Matrix44 operator -(const Matrix44 a, const Matrix44 b);
Matrix44 transposeMink(const Matrix44 m);

Matrix44 sanity(Matrix44 m);
const Matrix44 identity = {{1, 0, 0, 0,
                            0, 1, 0, 0,
                            0, 0, 1, 0,
                            0, 0, 0, 1}};
double insanity(Matrix44 m);

double form(Point p1, Point p2);
Point operator*(Matrix44 m, Point v);
Vector3 operator*(Matrix33 m, Vector3 v);
struct Absolute;
Point toNonPhysicalPoint (Absolute a );
struct Absolute {
    double x;
    double y;
    double z;
    Absolute move(Matrix44 m) {
        auto iii = m*toNonPhysicalPoint(*this);
        return {iii.x, iii.y, iii.z};
    }
}; /* {- ^ point on celestial sphere or "absolute point". t^2 = x^2 + y^2 + z^2
x^2+y^2+z^2 > 0-} */
Point toNonPhysicalPoint (Absolute a );

extern Matrix44 reflectAboutOrigin;
//moveAlongZ d = L.V4 (L.V4 1 0 0 0) (L.V4 0 1 0 0) (L.V4 0 0 (cosh d) (sinh d)) (L.V4 0 0 (sinh d) (cosh d))
//moveAlongY d = L.V4 (L.V4 1 0 0 0) (L.V4 0 (cosh d) 0 (sinh d)) (L.V4 0 0 1 0) (L.V4 0 (sinh d) 0 (cosh d))
//moveAlongX d = L.V4 (L.V4 (cosh d) 0 0 (sinh d)) (L.V4 0 1 0 0) (L.V4 0 0 1 0) (L.V4 (sinh d) 0 0 (cosh d))

Matrix44 moveAlongZ( double d);
Matrix44 moveAlongY( double d);
Matrix44 moveAlongX ( double d);

Matrix44 rotateAroundZ(double a);
Matrix44 rotateAroundY(double a);
Matrix44 rotateAroundX(double a);
Matrix33 moveAlongX3(double  d);
Matrix33 moveAlongY3(double  d);
Matrix33 rotate3(double  a);
const Vector3 origin3 = {0, 0, 1};
const Point origin = {0, 0, 0, 1};
Vector3 normalizeWass3 (Vector3 p);
inline Point normalizeWass (Point p) {
    auto d = sqrt (p.t*p.t - p.y*p.y - p.x*p.x - p.z*p.z) * sign(p.t);
    return {(p.x/d), (p.y/d), (p.z/d), (p.t/d)};
}
inline double form3 (Vector3 p1, Vector3 p2) {
    return p1.t*p2.t - p1.x*p2.x - p1.y*p2.y;
}
inline double distance3(Vector3 p1, Vector3 p2) {
    return acosh (form3 (normalizeWass3 (p1), normalizeWass3 (p2)));
}
inline double chDistance(Point a, Point b) {
    return - (form (normalizeWass (a), normalizeWass (b))); //let diff = a ^-^ b in form diff diff
}
inline double distance(Point a, Point b) {
    return (chDistance (a, b)) > 1? acosh (chDistance (a, b)) : 0;
}
inline Matrix33 moveTo3(Vector3 p, double d) {
    return rotate3(atan2 (p.y, p.x)) * moveAlongX3(d) * rotate3 (-(atan2( p.y, p.x)));
}
inline Matrix44 moveTo(Point xyzt, double d) {
    double x = xyzt.x;
    double y = xyzt.y;
    double z = xyzt.z;
//    double t = xyzt.t;
    double dem = x*x+y*y+z*z;
    return { ((cosh(d)*x*x+y*y+z*z)/dem),
                                  (((cosh(d)-1)*x*y)/dem),
                                  (((cosh(d)-1)*x*z)/dem),
                                  ((sinh(d)*x)/sqrt(dem)),
                                (((cosh(d)-1)*x*y)/(dem)),
                                  ((x*x+cosh(d)*y*y+z*z)/(dem)),
                                  (((cosh(d)-1)*y*z)/(dem)),
                                  ((sinh(d)*y)/sqrt(dem)),
                               (((cosh(d)-1)*x*z)/(dem)),
                                  (((cosh(d)-1)*y*z)/(dem)),
                                  ((x*x+y*y+cosh(d)*z*z)/dem),
                                  ((sinh(d)*z)/sqrt(dem)),
                              ((sinh(d)*x)/sqrt(dem)),
                                  ((sinh(d)*y)/sqrt(dem)),
                                  ((sinh(d)*z)/sqrt(dem)),
                                  (cosh(d))};
}
inline Matrix33 moveRightTo3 (Vector3 p) {
        return moveTo3( p, distance3( origin3, p));
}
inline Matrix44 moveRightTo (Point p) {
        return moveTo( p, distance( origin, p));
}
inline Matrix33 transposeMink3(const Matrix33 m) {
    Matrix33 r = transpose3(m);
    r(0, 2) *= -1;
    r(1, 2) *= -1;
    r(2, 0) *= -1;
    r(2, 1) *= -1;
    return r;
}
inline Matrix33 commute3(Matrix33 a, Matrix33 b) {
    return transposeMink3(a) * b * a;
}
inline Matrix44 commute(Matrix44 a, Matrix44 b) {
    return transposeMink(a) * b * a;
}
inline Matrix33 rotateAround3(Vector3 p, double  a) {
    return commute3 (transposeMink3( moveRightTo3( p)), rotate3(a));
}
const Matrix33 reflectOnX3 = {{1,  0, 0,
                         0, -1, 0,
                         0,  0, 1}};
const Matrix33 reflectOnY3 = {{-1,  0, 0,
                          0,  1, 0,
                          0,  0, 1}};
//Matrix33 reflect3(Vector3 p1, Vector3 p2) {
//    return commute3 (getSegmentToOx3( p1, p2), reflectOnX3);
//}
//Matrix44 moveToTangentVector(Vector3 a) {
//    return moveRightTo({ (cosh (a[0])), (cosh (a[1])), (cosh (a[2])), (sinh (a.norm()))}); // fixme это вроде неправильно
//}
inline Matrix33 moveToTangentVector3(Vector2 v) {
    double x = v[0], y = v[1];
    return moveTo3(  {(x), (y), (sqrt (x*x + y*y + 1))},  v.norm());
}
//normalizeKlein3 :: Floating a => L.V3 a -> L.V3 a
//normalizeKlein3 (L.V3 x y t) = L.V3 (x/t) (y/t) 1



//moveRightFromTo3 :: RealFloat a => L.V3 a -> L.V3 a -> L.M33 a
//moveRightFromTo3 p1 p2 = moveRightTo3 p1 <> moveRightTo3 (transposeMink3 (moveRightTo3 p1) L.!* p2) <> transposeMink3 ( moveRightTo3 p1)

//qr :: Floating a => L.M44 a -> L.M44 a
//qr {-(L.V4 (L.V4 a11 a12 a13 a14)
//         (L.V4 a21 a22 a23 a24)
//         (L.V4 a31 a32 a33 a34)
//         (L.V4 a41 a42 a43 a44) )-} = error "qr is not implemented"


// fast special invertions:
// (add inline pragmas...)
//invAroundY, invAroundZ :: (Num a) => L.M44 a -> L.M44 a
//invAroundZ (L.V4 (L.V4 a b _ _) _ _ _) = L.V4 (L.V4 a (-b) 0 0) (L.V4 b a 0 0) (L.V4 0 0 1 0) (L.V4 0 0 0 1)
//invAroundY (L.V4 (L.V4 a _ b _) _ _ _) = L.V4 (L.V4 a 0 (-b) 0) (L.V4 0 1 0 0) (L.V4 b 0 a 0) (L.V4 0 0 0 1)

//-- |4x4 matrix adjugate (inverse multiplied by det)
//-- for matrices with det 1 this is same as inL.V44 (modulo floating-point precision)
//-- (copied and patsed from linear package)
inline Matrix44 adj44(Matrix44 a) {
  double s0 = a(0, 0) * a(1, 1) - a(1, 0) * a(0, 1),
      s1 = a(0, 0) * a(1, 2) - a(1, 0) * a(0, 2),
      s2 = a(0, 0) * a(1, 3) - a(1, 0) * a(0, 3),
      s3 = a(0, 1) * a(1, 2) - a(1, 1) * a(0, 2),
      s4 = a(0, 1) * a(1, 3) - a(1, 1) * a(0, 3),
      s5 = a(0, 2) * a(1, 3) - a(1, 2) * a(0, 3),
      c5 = a(2, 2) * a(3, 3) - a(3, 2) * a(2, 3),
      c4 = a(2, 1) * a(3, 3) - a(3, 1) * a(2, 3),
      c3 = a(2, 1) * a(3, 2) - a(3, 1) * a(2, 2),
      c2 = a(2, 0) * a(3, 3) - a(3, 0) * a(2, 3),
      c1 = a(2, 0) * a(3, 2) - a(3, 0) * a(2, 2),
      c0 = a(2, 0) * a(3, 1) - a(3, 0) * a(2, 1);
  return { (a(1, 1) * c5 - a(1, 2) * c4 + a(1, 3) * c3),
           (-a(0, 1) * c5 + a(0, 2) * c4 - a(0, 3) * c3),
           (a(3, 1) * s5 - a(3, 2) * s4 + a(3, 3) * s3),
           (-a(2, 1) * s5 + a(2, 2) * s4 - a(2, 3) * s3),
           (-a(1, 0) * c5 + a(1, 2) * c2 - a(1, 3) * c1),
           (a(0, 0) * c5 - a(0, 2) * c2 + a(0, 3) * c1),
           (-a(3, 0) * s5 + a(3, 2) * s2 - a(3, 3) * s1),
           (a(2, 0) * s5 - a(2, 2) * s2 + a(2, 3) * s1),
           (a(1, 0) * c4 - a(1, 1) * c2 + a(1, 3) * c0),
           (-a(0, 0) * c4 + a(0, 1) * c2 - a(0, 3) * c0),
           (a(3, 0) * s4 - a(3, 1) * s2 + a(3, 3) * s0),
           (-a(2, 0) * s4 + a(2, 1) * s2 - a(2, 3) * s0),
           (-a(1, 0) * c3 + a(1, 1) * c1 - a(1, 2) * c0),
           (a(0, 0) * c3 - a(0, 1) * c1 + a(0, 2) * c0),
           (-a(3, 0) * s3 + a(3, 1) * s1 - a(3, 2) * s0),
           (a(2, 0) * s3 - a(2, 1) * s1 + a(2, 2) * s0)};
}

//{-

//*Main> distance (Point (-1.7252007297487297) (-14.389920392888179) 7.890168332835287 17.402341677747398) (Point (-1.7252007297487297) (-14.389920392888179) 7.890168332835287 17.402341677747398)
//NaN
//*Main> chDistance (Point (-1.7252007297487297) (-14.389920392888179) 7.890168332835287 17.402341677747398) (Point (-1.7252007297487297) (-14.389920392888179) 7.890168332835287 17.402341677747398)
//0.9999999999999982
//*Main>
//jjjjjgsgdxxxxxxx
//fd
//-}

//legByAdjacentAndOppositeAngles :: (Floating a) => a -> a -> a
//legByAdjacentAndOppositeAngles a b = acosh $ cos b / sin a

//hypotenuseByAngles :: (Floating a) => a -> a -> a
//hypotenuseByAngles a b = acosh $ recip $ tan a * tan b

inline double signedDistanceFromOxy(  Point p ) {
    return  distance( p, {p.x, p.y, 0, p.t}) * sign( p.z) * sign( p.t);
}
// that is, they must be linearly dependent

//pretty :: Show m => L.V4 (m) -> String
//pretty (L.V4 a b c d) = intercalate "\n" $ map show [a, b, c, d]

//proper :: (Num a, Ord a) => Point a -> Bool
//proper m = form m m < 0

//normalizeKlein :: Fractional a => Point a -> Point a
//normalizeKlein (Point x y z t) = Point (x/t) (y/t) (z/t) 1

//Matrix44 rotate(Point a, Point b, double x) {
//    return commute (andConsideringThat(getPointToOrigin (a),  turmPToOx, b), rotateAroundX x);
//}
//moveTol :: (RealFloat a) => Point a -> a -> ML a
//moveTol p dist = invert a <> (Dual [moveAlongX (dist)]) <> a
//    where a = (box . getPointToOxyAroundOx `andThen` box . getPointToOxzAroundOz)  p
////moveRightTo3 p@(L.V3 x y t) = rotate3 ((atan2 y x)) <> moveAlongX3 (distance3 origin3 p) <> rotate3 (-(atan2 y x))

//moveRightTo :: RealFloat a => Point a -> L.M44 a
//moveRightTo p = moveTo p (distance origin p)
//moveRightTol :: RealFloat a => Point a -> ML a
//moveRightTol p = moveTol p (distance origin p)


//moveFromTo :: (RealFloat a) => Point a -> Point a -> a -> L.M44 a
//moveFromTo fr to dist =   a <> moveTo (transposeMink a!$to) (dist) <> transposeMink a
//    where a = moveRightTo fr ////может быть, тут можно вместо moverightto использовать более простое движение - не из пяти, а из трёх элементарных

//turmPToOxz ::forall a . (Eq a, Floating a) => Point a -> L.M44 a // cbc fixme this function is same as getPointToOxzAroundOz ??
//turmPToOxz  (Point x y _ t) = rotateAroundZ alpha
//  where alpha = if(x/=0)then atan (-y/x) + ((signum (x*t) - 1)/2) * (-pi) else 0

//turmPToOx ::forall a . (Eq a, Floating a) => Point a -> L.M44 a // cbc fixme this function is same as getPointToOxyAroundOx ??
//turmPToOx  (Point x y z t) =  rotateAroundY (beta) <> rotateAroundZ alpha
//  where alpha = if(x/=0)then atan (-y/x) + ((signum (x*t) - 1)/2) * (-pi) else 0
//        beta = -signum t * (if (x /= 0 ) || (y /= 0) then atan (-z/sqrt(x*x + y*y)) else 0)
//// тут сожно наверное всё сделать ДРАМАТИЧЕСКИ быстрее, если вставить rewrite rules

////atan3 y x t = atan2 y x
//m33_to_m44M :: Num a => L.M33 a -> L.M44 a
//m33_to_m44M (L.V3 (L.V3 q w e) (L.V3 r t y) (L.V3 u i o)) = L.V4 (L.V4 q w 0 e) (L.V4 r t 0 y) (L.V4 0 0 1 0) (L.V4 u i 0 o)


//getPointToOrigin, getPointToOxzAroundOz, getPointToOxyAroundOx, getPointOnOxToOrigin :: forall a. RealFloat a => Point a -> L.M44 a
//getPointToOrigin = transposeMink . moveRightTo
//getPointToOxzAroundOz (Point x y _ t) = rotateAroundZ $ -(atan2 (y/t) (x/t)) ////  брать синус и косинус арктангенса очень весело, конечно
//getPointToOxyAroundOx (Point _ y z t) = rotateAroundX $ -(atan2 (z/t) (y/t)) //// от t нам нужен только знак, конечно, но я подозреваю, что лишний флоп лучше, чем лишнее ветвление
//getPointToOxyAroundOy (Point x _ z t) = rotateAroundY $ -(atan2 (z/t) (x/t)) //// FIXME FIXME тут угол я посчитал из предположения, что Y  направлена вправо, а может быть на самом деле она направлена влево и всё надо менять
//getPointOnOxToOrigin (Point x _ _ t) = moveAlongX $ asinh $ (  - x/ sqrt (( (t*t-x*x))) * signum t) //// брать гиперболические синус и косинус аркчосинуса очень весело, конечно
// moveFromTo a b d =
// getPointToOxyAroundOxl (Point _ y z t) = Dual [rotateAroundX $ -(atan2 (z/t) (y/t))]// // от t нам нужен только знак, конечно, но я подозреваю, что лишний флоп лучше, чем лишнее ветвление
//infixl 8 `andThen`
using ptm = Matrix44 (*)(Point);
inline Matrix44 andThen  (ptm f, ptm g, Point p) {// может быть, это какой-нибудь класс
  return g ((f (p)) * p) * f (p); // безумно неэффективно и вообще пиздец
}
using ptm3 = Matrix33 (*)(Vector3);
inline Matrix33 andThen3  (ptm3 f, ptm3 g, Vector3 p) {
    return g ((f (p)) * p) * f( p);
}
//andConsideringThat :: (Movable t m) => t -> (m -> t) -> m -> t
//andConsideringThat m f p = f (m !$ p) <> m

//andConsideringThat3 m f p = f (m L.!* p) <> m

// getTriangleToOxy a b c = (   (   getPointToOxyAroundOx `andThen`
//                                  getPointToOxzAroundOz `andThen`
//                                  getPointOnOxToOrigin $ a) `andConsideringThat`
//                              (   getPointToOxyAroundOx `andThen`
//                                  getPointToOxzAroundOz ) ) b `andConsideringThat`
//                          getPointToOxyAroundOx $ c
/*
getTriangleToOxy :: forall a.
                          RealFloat a =>
                          Point a -> Point a -> Point a -> L.M44 a
getTriangleToOxy a b c = (   (   getPointToOxyAroundOx `andThen`
                                 getPointToOxzAroundOz `andThen`
                                 getPointOnOxToOrigin $ a) `andConsideringThat`
                             (   getPointToOxyAroundOx `andThen`
                                 getPointToOxzAroundOz ) ) b `andConsideringThat`
                         getPointToOxyAroundOx $ c

getTriangleToOxyD :: forall a.
                           RealFloat a =>
                           Point a -> Point a -> Point a -> (Point a, Point a, Point a)
getTriangleToOxyD a b c = (t !$ a, t !$ b, t !$ c) where t = (getTriangleToOxy a b c) `seq` (getTriangleToOxy a b c)
*/
//  {-# INLINE mappend #-}

/*getSegmentToOx3 :: RealFloat a => L.V3 a -> L.V3 a -> L.M33 a
getSegmentToOx3 a b = getPointToOrigin3 a `andConsideringThat3` getPointToOxAroundO3 $ b*/
//getPointToOrigin3 :: RealFloat a => L.V3 a -> L.M33 a
//getPointToOrigin3 = transposeMink3 . moveRightTo3
//getPointToOxAroundO3 (L.V3 x y t) = rotate3 $ -(atan2 (y/t) (x/t))
/*
Мы хотим сделать, чтобы все углы треугольника стали на од




*/


//so much for agressive inlining... class methods dont get rewrited... class methods are harder to inline.. sad..


// instance Num a => Movable (Dual [(L.V4 (L.V4 a))]) (Absolute a) where
//   tran !$ (Abs a b c) = let (L.V4 am bm cm dm) = coerce tran L.!* (L.V4 a b c (a*a+b*b+c*c)) in Abs am bm cm
} // namespace H
#endif // HYPERBOLIC_H
