#include "hyperbolic.h"

H::Component &H::Point::operator[](int a) {
    return data[a];
}

H::Component &H::Vector3::operator[](int a) {
    return data[a];
}

double H::Vector3::norm() {
    return sqrt(x*x + y*y + t*t);
}

H::Component &H::Vector2::operator[](int a) {
    return data[a];
}

double H::Vector2::norm() {
    return sqrt(x*x + y*y);
}

H::Component &H::Matrix44::operator()(int i, int j) {
    return m[i*4+j];
}

H::Component H::Matrix44::operator()(int i, int j) const {
    return m[i*4+j];
}

//H::Matrix44 &H::Matrix44::operator *=(H::Matrix44 o) {
//    *this = *this * o;
//    return *this;
//    }

H::Component &H::Matrix33::operator()(int i, int j) {
    return m[i*3+j];
}

H::Component H::Matrix33::operator()(int i, int j) const {
    return m[i*3+j];
}

H::Matrix44 H::transpose(H::Matrix44 a) {
    return {{a(0,0), a(1,0), a(2,0), a(3,0),
                    a(0,1), a(1,1), a(2,1), a(3,1),
                    a(0,2), a(1,2), a(2,2), a(3,2),
                    a(0,3), a(1,3), a(2,3), a(3,3)
        }};
}

H::Matrix33 H::transpose3(H::Matrix33 a) {
    return {{a(0,0), a(1,0), a(2,0),
                    a(0,1), a(1,1), a(2,1),
                    a(0,2), a(1,2), a(2,2)}};
}



H::Vector3 H::klein(H::Point p) {
    //    Vector3 res;
    return { (p.x/p.t), (p.y/p.t), (p.z/p.t) };
}

H::Matrix44 H::operator *(const H::Matrix44 a, const H::Matrix44 b) {
    Matrix44 r;
    for(int i : {0, 1, 2, 3}) {
        for(int j : {0, 1, 2, 3}) {
            r(i, j) = 0;
            for(int k : {0, 1, 2, 3}) {
                r(i, j) += a(i, k)*b(k, j);
            }
        }
    }
    return r;
}

H::Matrix33 H::operator *(const H::Matrix33 a, const H::Matrix33 b) {
    Matrix33 r;
    for(int i : {0, 1, 2}) {
        for(int j : {0, 1, 2}) {
            r(i, j) = 0;
            for(int k : {0, 1, 2}) {
                r(i, j) += a(i, k)*b(k, j);
            }
        }
    }
    return r;
}

H::Vector3 H::cross(H::Vector3 p, H::Vector3 r) {
    return {{ (p[1]*r[2]-p[2]*r[1]), (p[2]*r[0]-p[0]*r[2]), (p[0]*r[1]-p[1]*r[0])}};
}

double H::dot(H::Vector3 a, H::Vector3 b) {
    double r = 0;
    FOR3(j) {
        r+=a[j]*b[j];
    }
    return r;
}

H::Matrix44 H::inv44(H::Matrix44 a) {
    double s0 = a(0, 0) * a(1, 1) - a(1, 0) * a(0, 1);
    double       s1 = a(0, 0) * a(1, 2) - a(1, 0) * a(0, 2);
    double s2 = a(0, 0) * a(1, 3) - a(1, 0) * a(0, 3);
    double                 s3 = a(0, 1) * a(1, 2) - a(1, 1) * a(0, 2);
    double          s4 = a(0, 1) * a(1, 3) - a(1, 1) * a(0, 3);
    double               s5 = a(0, 2) * a(1, 3) - a(1, 2) * a(0, 3);
    double        c5 = a(2, 2) * a(3, 3) - a(3, 2) * a(2, 3);
    double                 c4 = a(2, 1) * a(3, 3) - a(3, 1) * a(2, 3);
    double          c3 = a(2, 1) * a(3, 2) - a(3, 1) * a(2, 2);
    double   c2 = a(2, 0) * a(3, 3) - a(3, 0) * a(2, 3);
    double                 c1 = a(2, 0) * a(3, 2) - a(3, 0) * a(2, 2);
    double          c0 = a(2, 0) * a(3, 1) - a(3, 0) * a(2, 1);
    double   det = s0 * c5 - s1 * c4 + s2 * c3 + s3 * c2 - s4 * c1 + s5 * c0;
    return Matrix44{ (a(1, 1) * c5 - a(1, 2) * c4 + a(1, 3) * c3),
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
                (a(2, 0) * s3 - a(2, 1) * s1 + a(2, 2) * s0)} / det;
}

H::Matrix44 H::operator -(const H::Matrix44 a, const H::Matrix44 b) {
    Matrix44 r;
    for(int i : {0, 1, 2, 3}) {
        for(int j : {0, 1, 2, 3}) {
            r(i, j) = a(i, j) - b(i, j);
        }
    }
    return r;
}

H::Matrix44 H::transposeMink(const H::Matrix44 m) {
    Matrix44 r = transpose(m);
    r(0, 3) *= -1;
    r(1, 3) *= -1;
    r(2, 3) *= -1;
    r(3, 0) *= -1;
    r(3, 1) *= -1;
    r(3, 2) *= -1;
    return r;
}

H::Matrix44 H::sanity(H::Matrix44 m) {
    return m * transposeMink(m);
}

double H::insanity(H::Matrix44 m) {
    int r=0;
    Matrix44 mat = sanity(m)-identity;
    for(int i : {0, 1, 2, 3}) {
        for(int j : {0, 1, 2, 3}) {
            r += mat(i, j)*mat(i, j);
        }
    }
    return r;
}

double H::form(H::Point p1, H::Point p2) /*{- fundamental minkowski form, она зависит от координатного
представления точки, то есть не однозначна для точек гиперболического пространства,
её стоит использовать с осторожностью -}*/ {
    return p1.x*p2.x + p1.y*p2.y + p1.z*p2.z - p1.t*p2.t;
}

H::Point H::operator*(H::Matrix44 m, H::Point v) {
    Point r;
    for(int i : {0, 1, 2, 3}) {
        r[i] = 0;
        FOR4(j) {
            r[i] += v[j] * m(i, j);
        }
    }
    return r;
}

H::Vector3 H::operator*(H::Matrix33 m, H::Vector3 v) {
    Vector3 r;
    for(int i : {0, 1, 2}) {
        r[i] = 0;
        FOR3(j) {
            r[i] += v[j] * m(i, j);
        }
    }
    return r;
}

H::Point H::toNonPhysicalPoint(H::Absolute a) {
    return {a.x, a.y, a.z, ((a.x*a.x)+(a.y*a.y)+(a.z*a.z))};
}

H::Matrix44 H::reflectAboutOrigin = {{-1.0, 0, 0, 0,
                                0, -1.0, 0, 0,
                                0, 0, -1.0, 0,
                                0, 0, 0, 1.0}};

H::Matrix44 H::moveAlongZ(double d) {
    return {{1, 0, 0, 0,
            0, 1, 0, 0,
            0, 0, cosh(d), sinh(d),
                    0, 0, sinh(d), cosh(d)}};
}

H::Matrix44 H::moveAlongX(double d) {
    return {{cosh (d), 0, 0, sinh( d),
                    0, 1, 0, 0,
                    0, 0, 1, 0,
             sinh (d), 0, 0, cosh (d)}};
}

H::Matrix44 H::moveAlongY(double d) {
    return {{1, 0, 0, 0,
            0, cosh (d), 0, sinh(d),
                    0, 0, 1, 0,
                    0, sinh(d), 0, cosh (d)}};
}

H::Matrix44 H::rotateAroundZ(double a) {
    return {{cos (a), -sin( a), 0, 0,
                    sin (a), cos( a), 0, 0,
                    0, 0, 1, 0,
                    0, 0, 0, 1}};
}

H::Matrix44 H::rotateAroundY(double a) {
    return {{cos (a), 0, sin( a), 0,
                   0, 1,       0, 0,
             -sin(a), 0, cos (a), 0,
                   0, 0,       0, 1}};
}

H::Matrix44 H::rotateAroundX(double a) {
    return {{1, 0, 0, 0,
            0, cos (a), -sin( a), 0,
                    0, sin (a), (cos (a)), 0,
                    0, 0, 0, 1}};
}

H::Matrix33 H::moveAlongX3(double d) {
    return {{cosh (d), 0, sinh( d),
                    0, 1, 0,
                    sinh (d), 0, cosh (d)}};
}

H::Matrix33 H::moveAlongY3(double d) {
    return {{1, 0, 0,
            0, cosh (d), sinh( d),
                    0, sinh (d), cosh (d)}};
}

H::Matrix33 H::rotate3(double a) {
    return {{cos(a), -sin(a), 0,
                    sin(a),  cos(a), 0,
                    0,            0, 1}};
}

H::Vector3 H::normalizeWass3(H::Vector3 p) {
    auto d = sqrt (p.t*p.t - p.y*p.y - p.x*p.x) * sign(p.t);
    return {(p.x/d), (p.y/d), (p.t/d)};
}