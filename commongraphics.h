#ifndef COMMONGRAPHICS_H
#define COMMONGRAPHICS_H
#include "SDL2/SDL_video.h"
#include "SDL2/SDL.h"
#include "util/hyperbolic.h"
//#include "/usr/include/old-array.h"
#include <array>
#include "GL/freeglut.h"
#include "util/physics.h"
#include "util/lodepng.h"
extern SDL_Window* window;
extern SDL_GLContext context;

extern "C" GLAPI void APIENTRY glWindowPos2f (GLfloat x, GLfloat y);

namespace G {
inline H::Matrix44 perspective(double fovy, double aspect, double near, double far) {
    double tanHalfFovy = tan( fovy / 2);
    double x = 1 / (aspect * tanHalfFovy);
    double y = 1 / tanHalfFovy;
    double fpn = far + near;
    double           fmn = far - near;
    double           oon = 0.5/near;
    double           oof = 0.5/far;
//          -- z = 1 / (near/fpn - far/fpn) -- would be better by .5 bits
        double       z = -fpn/fmn;
         double      w = 1/(oof-oon); //-- 13 bits error reduced to 0.17
 //        -- w = -(2 * far * near) / fmn
  return {{x, 0, 0,    0,
           0, y, 0,    0,
           0, 0, z,    w,
           0, 0, (-1), 0}};
}
const H::Matrix44 persMatrix = perspective( (H::tau/8), (1366.0/768), (0.01), (1));
inline bool nearZero(double f) {
    return fabs (f) <= 1e-12;
}
inline H::Vector3 normalize(H::Vector3 v) {
    auto l = dot(v,v);
    return  nearZero (l) || nearZero (1-l) ? v : fmap (([l](auto x){ return (x/sqrt (l));}),  v);
}
inline H::Matrix44 lookAt(H::Vector3 eye, H::Vector3  center, H::Vector3 up) {
    auto za = normalize (center - eye);
    auto xa = normalize (cross (za, up));
    auto ya = cross(xa, za);
    auto xd = -dot( xa, eye);
    auto yd = -dot(ya, eye);
    auto zd = dot(za, eye);
    return {{(xa.x), (xa.y), (xa.z),  xd,
             (ya.x), (ya.y), (ya.z),  yd,
            (-za.x), (-za.y), (-za.z), zd,
                0, 0, 0, 1}};
}
const H::Matrix44 viewMatrix = lookAt ({{(0), 0, 0}},  {{(1), (0), (0)}},  {(0.0), 0, (1)});
const H::Matrix44 persViewMatrix = persMatrix  * viewMatrix;
inline H::Point saneVertex4 (H::Point a)  {
    return a.t*a.t >= 0 ? a : (-a);
}
extern int width , height ;
void  initialiseGraphics(int sg, char** hr);
const Mesh transparentDeviator();

const Mesh deviator(double size);
const Mesh sourceMesh();
inline double clamp(double a) {
    return a > 1?1:a<0?0:a;
}
void lightenABit(Mesh *d);
template <typename F>
void renderPrimitive(GLenum p, F f) {
    glBegin(p);
    f();
    glEnd();
}
} // namespace G
#endif // COMMONGRAPHICS_H

