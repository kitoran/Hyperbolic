#include "physics.h"
AvatarPosition toAvatarPosition(const H::Matrix44 &st) {
    Point poi = st * origin;
    double h = distance(poi, {poi.x, poi.y, 0, poi.t});
    // вроде бы это acosh(poi.z/poi.t)
    Matrix33 pos = isometryByOriginAndOx3(
                projectToOxy(poi),
                projectToOxy(st * Point{sinh(1), 0, 0, cosh(1)})
                );
    Vector3 deb = pos*origin3;
//    Matrix33 pos = {{ st(0,0), st(0,1), st(0,3),
//                      st(1,0), st(1,1), st(1,3),
//                      st(3,0), st(3,1), st(3,3)}};
    return {pos, h, 0, {0,0,0}};
}
Matrix44 fromAvatarPosition(const AvatarPosition &ap) {
    // обратно к G::viewPort
    return  (H::m33_to_m44M (ap.pos)) * H::moveAlongZ (ap.height) * H::rotateAroundY (ap.nod);
}



Matrix44 sqrtPushOutStandardRectangle(double width, double height, const Point &moved) {

    Point projection = moved; projection.z=0; // z pf,sk gjxtve 'nj nfr ghjcnj
//    double distToOxy = distance(moved, projection);
    Point projectionX = projection; projectionX.x = 0;
    double distToX = distance(projectionX, projection);
    Point projectionY = projection; projectionX.y = 0;
    double distToY = distance(projectionY, projection);
    bool farFromX = distToX >= height/2 + ourSize;
    bool closeToX = distToX < height/2;
    bool farFromY = distToY >= width/2 + ourSize;
    bool closeToY = distToY < width/2;

    //    10 случаев
    if(farFromX || farFromY) return identity; // 1 случай
    if(!closeToX && !closeToY) {
        // 4 случая
        double movex = copysign(width/2, projection.x);
        double movey = copysign(height/2, projection.y);
        Point corner = moveAlongX(movex) * moveAlongY(movey) * origin;
        // хотя в произведении важен порядок множителей, в данном случае точка будет та же самая
        double distanceToCorner = distance(moved, corner);
        if(distanceToCorner > ourSize) {
            return identity;
        } else {
            return moveFromTo(corner, moved, ourSize-distanceToCorner);
        }
    }
    if(closeToX && !closeToY) {
        double movey = copysign(height/2, projection.y);
        Point movedFurther = moveAlongY(-movey) * moved;
        Point projectedFurther = {movedFurther.x, 0,0, movedFurther.t};
        double distanceToEdge = distance(movedFurther, projectedFurther);
        if(distanceToEdge > ourSize) {
            return identity;
        } else {
            return moveFromTo(projectedFurther, moved, ourSize-distanceToEdge);
        }
    }
    if(closeToY && !closeToX) {
        double movex = copysign(height/2, projection.x);
        Point movedFurther = moveAlongY(-movex) * moved;
        Point projectedFurther = movedFurther;
        projectedFurther.x = 0; projectedFurther.z = 0;
        double distanceToEdge = distance(movedFurther, projectedFurther);
        if(distanceToEdge > ourSize) {
            return identity;
        } else {
            return moveFromTo(projectedFurther, moved, ourSize-distanceToEdge);
        }
    }
    return moveFromTo(projection, moved, ourSize-distance(projection, moved));
}
Matrix44 sqrtPushOutRectangle(const Matrix44 &m, double width, double height, const Point &p) {
    // width and height в самой узкой части меряются т е это расстояния между соотв сторонами
    // если они слишком большие и не образуют четырехугольник то хз че
    Point moved = transposeMink(m)*p;//currentPosition(s);
    Matrix44 r = sqrtPushOutStandardRectangle(width, height, moved);
    if(r == identity) return identity;
    return transposeMink(m)*r*m;
}
