#ifndef COMMONGRAPHICS_H
#define COMMONGRAPHICS_H
#include "SDL2/SDL_video.h"
#include "SDL2/SDL.h"
#include "hyperbolic.h"
#include <array>
#include "GL/freeglut.h"
extern SDL_Window* window;
extern SDL_GLContext context;
namespace G {
constexpr H::Matrix44 perspective(double fovy, double aspect, double near, double far) {
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
constexpr H::Matrix44 persMatrix = perspective( (H::tau/8), (1024/600), (0.01), (1));
inline bool nearZero(double f) {
    return abs (f) <= 1e-12;
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
inline void  initialiseGraphics(int sg, char** hr) {
        glutInit(&sg, hr);
        SDL_Init(SDL_INIT_VIDEO);

        SDL_DisplayMode display;
        SDL_GetCurrentDisplayMode(0, &display);

        SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
        width = display.w; height = display.h;
        window = SDL_CreateWindow("Hyperbolic",  0, 0, display.w, display.h, SDL_WINDOW_BORDERLESS | SDL_WINDOW_OPENGL);
        context = SDL_GL_CreateContext(window);

        glDepthFunc( GL_LESS );

        glDisable(GL_DEPTH_CLAMP);

        glEnable(GL_LINE_SMOOTH);
        glLineWidth(2);

        glEnable(GL_POLYGON_OFFSET_FILL);
        glPolygonOffset(-0.2, 1);

        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        auto cursor = SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_CROSSHAIR);
        SDL_SetCursor(cursor);
        SDL_ShowWindow(window);
}
} // namespace G
#endif // COMMONGRAPHICS_H
