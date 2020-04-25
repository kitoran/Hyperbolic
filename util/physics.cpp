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

