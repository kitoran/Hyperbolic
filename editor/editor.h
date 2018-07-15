#ifndef EDITOR_H
#define EDITOR_H
#include "SDL2/SDL.h"
#define GL_GLEXT_PROTOTYPES
#include "GL/gl.h"
#include "float.h"
#include "assert.h"
#include "util/hyperbolic.h"
#include "util/commongraphics.h"
#include "util/physics.h"
#include <map>
#include <iostream>
const double step = 0.1;
using namespace H;
Matrix44 view = rotateAroundY (0.1) /* (tau/(4-0.1))*/  * moveAlongX (-0.1) /* moveAlongZ (-0.1) !*! ) -*/ * identity;
std::map<SDL_Keycode, H::Matrix44> matricesMove =
 {{SDLK_w, moveAlongX (-step)}, {SDLK_s, moveAlongX(step)},
   {SDLK_a, moveAlongY (-step)}, {SDLK_d, moveAlongY(step)},
  {SDLK_z, moveAlongZ  (-step)}, {SDLK_c, moveAlongZ (step)}};

void keyboardCase (SDL_KeyboardEvent a) {
    SDL_Keycode c = a.keysym.sym;
    auto f = matricesMove.find(c);
    if (f != matricesMove.end()){
      view = f->second * view; /* 0.1 */;
    }
}
auto fromGradi(double x) {
    return x / 360*tau*7.0/30.0;
}
enum SelectedThingType {Nihil, Mes, Obs};
struct SelectedThing {
    SelectedThingType type = Nihil;
    static constexpr const char *const TypeNames[] = {"Nihil", "Mes", "Obs"};
    int32_t n = -1;
} selectedThing;
void renderPrimitive(GLenum p, auto f) {
    glBegin(p);
    f();
    glEnd();
}
enum ExplicitObjectType {Me, So, Re};
struct ExplicitObject {
    ExplicitObjectType type;

       Mesh mesh = {};
       Source source = {};
       Receiver receiver = {};

};
double clamp(double a) {
    return a > 1?1:a<0?0:a;
}
struct Scene {
    std::map<int32_t, ExplicitObject> ex;
    std::map<int32_t, Obstacle> im;
};
Scene mmmm() {
    std::vector<H::Point> v;
    for( double i : {0, 1, 2, 3} ) {
        auto ni = i*tau;
        auto bbi = ni/4;
        Point p = rotateAroundZ(bbi-0.1) * moveAlongZ(-0.1) * Point {0.9999, 0, 0, 1};
        v.push_back(p);
    }
//    Point ppp = {.2, .2, .2, 1};
//    Point ppn = {.2, .2, -.2, 1};
//    Point pnp = {.2, -.2, .2, 1};
//    Point pnn = {.2, -.2, -.2, 1};
//    Point npp = {-.2, .2, .2, 1};
//    Point npn = {-.2, .2, -.2, 1};
//    Point nnp = {-.2, -.2, .2, 1};
//    Point nnn = {-.2, -.2, -.2, 1};
//    ColoredEntity p1 = {{0, 1, 1, 1}, {Polygon, {ppp, ppn, pnn, pnp}}};
//    ColoredEntity n1 = {{1, 1, 1, 1}, {Polygon, {npp, npn, nnn, nnp}}};
//    ColoredEntity p2 = {{1, 1, 1, 1}, {Polygon, {ppp, ppn, npn, npp}}};
//    ColoredEntity n2 = {{1, 1, 1, 1}, {Polygon, {pnp, pnn, nnn, nnp}}};
//    ColoredEntity p3 = {{1, 1, 1, 1}, {Polygon, {ppp, pnp, nnp, npp}}};
//    ColoredEntity n3 = {{1, 1, 1, 1}, {Polygon, {ppn, pnn, nnn, npn}}};
    HyperEntity he;
    he.type = Polygon;
    he.p = v;
    ColoredEntity ce = {{0.9, 0.1, 1, 1.0}, he};
    Mesh m = {ce, };
    ExplicitObject e = {Me, m};
    return Scene{
        std::map<int32_t, ExplicitObject>{
            std::pair<int32_t, ExplicitObject>{ 0x44ff3388, e}
        }, {}};
}
enum State { Ground, AddingWall } state;
Scene scene = mmmm();
using Angle = double;
Vector2 mapVertexPixel(Vector2 ab) {
    return {((ab[0])*2/(G::width) - 1 ),
                                        (1 - (ab[1])*2/(G::height) )};
}
using namespace G;
Mesh beingAddedWall(Angle /*a*/, Vector2 pos) {
    auto xy =  mapVertexPixel( pos);
    auto tran = G::persViewMatrix * view;
    auto ijkl = tran.m+12;
    auto c = -(ijkl[3]+ijkl[0]*xy[0]+ijkl[1]*xy[1])/ijkl[2];
    auto invpv =  inv44  (tran);
    Point d = invpv * (abs ( ijkl[2]) > 0.00001 ? Point{ xy[0], xy[1],  c, 1} : Point{ 0, 0, 1, 0});
//    auto res = tran !* d, L.V2 x y)
    Mesh wall {{ {0.0, 0.0, 1.0, 1.0}, {Polygon, {{0, 0.01, 0, 1},
                                                     {0, (-0.01), 0, 1},
                                                     {0, (-0.01), 0.01, 1},
                                                     {0, 0.01, 0.01, 1}
                                                       }, origin, origin}}};
//    assert (abs (rz) < 0.04) ;
    return H::moveRightTo (d) * wall;

}
struct Button {
    const char* text;
//    Vector2 _pos;
    bool(*active)();
    void(*action)();
};
bool showMainAxes = true;
std::vector<Button> gui = { {"wall", [](){return state == Ground;}, ( [](){state = AddingWall;} )},
                            {"main axes", [](){return true;}, ( [](){ showMainAxes = !showMainAxes;} )}};
struct Rectangle {
    double lx;
    double ly;
    double sx;
    double sy;
    bool contains(double x, double y) {
        return lx <= x && ly <= y && (lx+sx) >= x && (ly+sy) >= y;
    }
};
void mapPixelVertex(double a, double b) {
    glVertex2f((a)*2/(width) - 1 ,
                                             (1 - (b)*2/(height) ));
}
void displayRectangle(Rectangle r) {
//displayRectangle (SDL.Rectangle (SDL.P (V2 lx ly)) (V2 sx sy)) =
    renderPrimitive(GL_QUADS, [&](){
        mapPixelVertex(r.lx, r.ly);
        mapPixelVertex(r.lx+r.sx, r.ly);
        mapPixelVertex(r.lx+r.sx, r.ly+r.sy);
        mapPixelVertex(r.lx, r.ly+r.sy);
    });
}
auto margin = 3;
Rectangle buttRectangle (const Button & butt, int number/*text (Pos a b) _ _ _*/) {
  double i = glutBitmapLength(GLUT_BITMAP_TIMES_ROMAN_24, (const unsigned char*)butt.text);
  double h = glutBitmapHeight(GLUT_BITMAP_TIMES_ROMAN_24);
  double x  = 300;
  double y = 200 + number*h+2*margin*number;
  return Rectangle{(x), (y), (i+2*margin), ((h + 2*margin))};
}
extern "C" GLAPI void APIENTRY glWindowPos2f (GLfloat x, GLfloat y);
void displayButton (Button butt, int number) {
    double h = glutBitmapHeight(GLUT_BITMAP_TIMES_ROMAN_24);
    double x  = 300;
    double y = 200 + number*h+2*margin*number;
    glColor3f(.5, .5, .5);
    displayRectangle(buttRectangle(butt, number));
    glColor3f(1, 1, 1);
    auto debug = x + margin;
    glWindowPos2f(x + margin, height - (y + h + margin));
    glColor3f(0, 1, 0);
    glutBitmapString(GLUT_BITMAP_TIMES_ROMAN_24, (const unsigned char*)butt.text);
}
Mesh xarrow(double size, bool transparent) {
    Point pp = {-size, size/2, size/2, 1};
    Point pn = {-size, size/2, -size/2, 1};
    Point nn = {-size, -size/2, -size/2, 1};
    Point np = {-size, -size/2, size/2, 1};
    return moveAlongX(1) * Mesh{{{1, 0, 0, transparent?0.3f:1}, {Polygon, {origin, pp, pn}}},
                                {{1, 0, 0, transparent?0.3f:1}, {Polygon, {origin, pn, nn}}},
                                {{1, 0, 0, transparent?0.3f:1}, {Polygon, {origin, nn, np}}},
                                {{1, 0, 0, transparent?0.3f:1}, {Polygon, {origin, np, pp}}},
                        {{1,1,1,transparent?0.3f:1}, {Segment, {}, origin, pp}},
                            {{1,1,1,transparent?0.3f:1}, {Segment, {}, origin, pn}},
                                {{1,1,1,transparent?0.3f:1}, {Segment, {}, origin, nn}},
                                    {{1,1,1,transparent?0.3f:1}, {Segment, {}, origin, np}}};
}
Mesh yarrow(double size, bool transparent) {
    Point pp = { size/2,-size, size/2, 1};
    Point pn = {size/2, -size, -size/2, 1};
    Point nn = {-size/2, -size, -size/2, 1};
    Point np = {-size/2, -size, size/2, 1};
    return moveAlongY(1) * Mesh{{{0,1,  0, transparent?0.3f:1}, {Polygon, {origin, pp, pn}}},
                                {{0,1,  0, transparent?0.3f:1}, {Polygon, {origin, pn, nn}}},
                                {{0,1,  0, transparent?0.3f:1}, {Polygon, {origin, nn, np}}},
                                {{0,1,  0, transparent?0.3f:1}, {Polygon, {origin, np, pp}}},
                        {{0,0,0,transparent?0.3f:1}, {Segment, {}, origin, pp}},
                            {{0,0,0,transparent?0.3f:1}, {Segment, {}, origin, pn}},
                                {{0,0,0,transparent?0.3f:1}, {Segment, {}, origin, nn}},
                                    {{0,0,0,transparent?0.3f:1}, {Segment, {}, origin, np}}};
}
Mesh zarrow(double size, bool transparent) {
    Point pp = {size/2,  size/2,-size, 1};
    Point pn = {size/2,  -size/2,-size, 1};
    Point nn = {-size/2,  -size/2,-size, 1};
    Point np = {-size/2,  size/2,-size, 1};
    return moveAlongZ(1) * Mesh{{{0,0,1, transparent?0.3f:1}, {Polygon, {origin, pp, pn}}},
                                {{0,0,1, transparent?0.3f:1}, {Polygon, {origin, pn, nn}}},
                                {{0,0,1, transparent?0.3f:1}, {Polygon, {origin, nn, np}}},
                                {{0,0,1, transparent?0.3f:1}, {Polygon, {origin, np, pp}}},
                        {{0,0,0,transparent?0.3f:1}, {Segment, {}, origin, pp}},
                            {{0,0,0,transparent?0.3f:1}, {Segment, {}, origin, pn}},
                                {{0,0,0,transparent?0.3f:1}, {Segment, {}, origin, nn}},
                                    {{0,0,0,transparent?0.3f:1}, {Segment, {}, origin, np}}};
}
void editorDisplay() {
    auto applyNormal = [](auto )->void {
        /*auto a = l[0];
        auto b = l[1];
        auto c = l[2];
        * пока не нужно L.V3 x1 y1 z1 = signorm $ cross ( klein a-klein b ) (klein a - klein c)
        auto norm = if z1 < 0 then V3 x1 y1 z1 else negate (V3 x1 y1 z1)
        GLnormal3d(norm); */

    };
    auto transform = [](Point p) {
        std::cout << "x = " << p.x << "y = " << p.y << "z = "  << p.z << "t = "  << p.t << std::endl;
        Point a = G::persViewMatrix * ( view * p);
        glVertex4dv(G::saneVertex4(a).data);
    };
    glPointSize(20);
    auto toRaw = [&transform, &applyNormal](bool bb, int32_t qwq, const ColoredEntity& ce) {
        if(ce.e.type == Polygon) {
            renderPrimitive(GL_POLYGON, [&transform, &applyNormal, bb, qwq, &ce](){

                if(bb) {
                    glColor4f(clamp(ce.color.r + 0.3), clamp(ce.color.g + 0.3), clamp(ce.color.b + 0.3), clamp(ce.color.a + 0.3));
                } else {
                    glColor4fv(ce.color.m);
                }
                applyNormal(ce.e.p);

                for(auto x:ce.e.p) {
                    transform(x);
                }
            });
            glDisable(GL_DEPTH_TEST);
            renderPrimitive(GL_POINTS, [&transform, &applyNormal, bb, qwq, &ce](){
                glColor4f(1, 1, 1, 1);
                for(auto x:ce.e.p) {
                    transform(x);
                }
            });
            glEnable(GL_DEPTH_TEST);

        } else if(ce.e.type == Segment) {
            renderPrimitive(GL_LINES, [&](){
                glColor4fv((float*)(&(ce.color)));
                transform(ce.e.a);
                transform(ce.e.b);
            });
        } else {
            renderPrimitive(GL_POINTS, [&](){
                glColor4dv((double*)(&(ce.color)));
                transform(ce.e.a);
            });
        }
    };
    auto rend = [&](int32_t m, const ExplicitObject &e) {
        if(e.type == Me) {
            if(selectedThing.type == Mes && selectedThing.n == m) {
                for(auto x = e.mesh.begin(); x < e.mesh.end(); x++) {
                    toRaw(true, m, *x);
                }
            } else {
                for(auto x = e.mesh.begin(); x < e.mesh.end(); x++) {
                    toRaw(false, m, *x);
                }
            }
        }
        else {
            abort();
        }

    };
//    auto frame = [&](HyperEntity h) {
//        if(h.type == Polygon) {
//            renderPrimitive(GL_LINE_LOOP,  [&]() {
//                for(auto x : h.p) {
//                    transform (x);
//                }
//            }
//            );

//        }
//    };
    glClearColor(0,0,0,1);
        glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        for(auto& i : scene.ex) {
            rend( i.first, i.second);
        }
        if(showMainAxes) {
            renderPrimitive(GL_LINES, [&](){

                glColor4d(1, 0, 0, 1);
                transform({-1, 0, 0, 1});
                transform({1, 0, 0, 1});
                glColor4d(0, 1, 0, 1);
                transform({0, -1, 0, 1});
                transform({0, 1, 0, 1});
                glColor3b(0, 0, 127);
                transform({0, 0, -1, 1});
                transform({0, 0, 1, 1});
            });
            rend(0, {Me, xarrow(0.06, false)});
            rend(0, {Me, yarrow(0.06, false)});
            rend(0, {Me, zarrow(0.06, false)});
            glClear(GL_DEPTH_BUFFER_BIT);
            renderPrimitive(GL_LINES, [&](){

                glColor4d(1, 0, 0, 0.3);
                transform({-1, 0, 0, 1});
                transform({1, 0, 0, 1});
                glColor4d(0, 1, 0, 0.3);
                transform({0, -1, 0, 1});
                transform({0, 1, 0, 1});
                glColor4d(0, 0, 1, 0.3);
                transform({0, 0, -1, 1});
                transform({0, 0, 1, 1});
            });
            rend(0, {Me, xarrow(0.06, true)});
            rend(0, {Me, yarrow(0.06, true)});
            rend(0, {Me, zarrow(0.06, true)});

        }
        if(state == AddingWall) {
            int x, y;
            SDL_GetMouseState(&x, &y);
            for(auto fwe : beingAddedWall(0, {double(x), double(y)})) {
                toRaw(false, -1, fwe);
            }
        }
        glColor4d( 1, 1, (1), 1);
        renderPrimitive( GL_TRIANGLES, []() {
            double x = 0.5, y = 0, z = 0, t = 1;
            glVertex4dv(saneVertex4({x-0.01, y, z, t}).data);

            glVertex4dv(saneVertex4({x,(y-0.01), z, t}).data);
            glVertex4dv(saneVertex4({x, y, (z-0.01), t}).data);
        });

        glDisable(GL_DEPTH_TEST);
        glDepthFunc(GL_ALWAYS);

        for(int i = 0; i < gui.size(); i++) {
            displayButton(gui[i], i);
        }

        glDepthFunc(GL_LESS);
        glEnable(GL_DEPTH_TEST);

        SDL_GL_SwapWindow(window);
//    FOR4(i) {
//        FOR4(j) {
//            std::cout <<  view(i,j) << " ";
//        }
//        std::cout << "\n";
//    }
    std::cout << insanity(view) << " " << insanity(identity) << insanity(moveAlongX (-0.1) * identity)  << std::endl;

}
int32_t selected(double xx, double yy) {

    double depth = DBL_MAX;
    int32_t buffer;

        auto toRaw = [&](int32_t qwq, const ColoredEntity& ce) {
            if(ce.e.type == Polygon) {
                unsigned char* e = (GLubyte*)(&qwq);
                unsigned char e4[4] = {e[0], e[1], e[2], e[3]};
                Point a = G::persViewMatrix * ( view * ce.e.p[ce.e.p.size()-1]);
                Vector2 last = {a.x/a.t, a.y/a.t};
//                bool f0 = false;
//                Vector3 v0;
//                bool f1 = false;
//                Vector3 v1;
//                bool f2 = false;
//                Vector3 v2;
                bool neg = false;
                bool pos = false;
                bool visible = false;
                bool infinite = false;
                Vector2 startingHui;
                Vector2 endingHui;
                bool linvert = fabs(a.z/a.t) > 1;
                for(int i = 0; i < ce.e.p.size(); i++) {
                    Point a = G::persViewMatrix * ( view * ce.e.p[i]);
                    Vector2 newp {a.x/a.t, a.y/a.t};
                    bool invert = fabs(a.z/a.t) > 1;
//                    Vector2 ortho;
//                    Vector2 thing;
                    if(!invert) visible = true;
                    else infinite = true;
                    if(invert && !linvert) {
                        Vector2 ortho = {-newp.y + last.y, newp.x-last.x};
                        Vector2 thing = {xx - last.x, yy - last.y};
                        auto e = ortho*thing;
                        linvert = invert;
                        if(e>0) pos = true; else neg = true;
                        endingHui = {newp.x - last.x, newp.y - last.y};

                    } else if(linvert && !invert) {
                        startingHui = {-newp.x + last.x, -newp.y + last.y};
                        Vector2 ortho = {-newp.y + last.y, newp.x-last.x};
                        Vector2 thing = {xx - newp.x, yy - newp.y};
                        auto e = ortho*thing;
                        linvert = invert;
                        if(e>0) pos = true; else neg = true;
                    } else if(linvert && invert) {

                    } else {
                        Vector2 ortho = {newp.y - last.y, -newp.x+last.x};
                        Vector2 thing = {xx - newp.x, yy - newp.y};
                        auto e = ortho*thing;
                        if(e>0) pos = true; else neg = true;
                    }
                    last = newp;
//                    auto e = ortho*thing;
//                    if(invert ) e *= -1;
//                    if(linvert ) e *= -1;
//                    if(e>0) pos = true; else neg = true;
                }
                if(infinite) {
                    auto e = pseudoscalar(startingHui, endingHui);
                    if(e>0) pos = true; else neg = true;
                }
//                Vector2 coords = inv22({{(v1.x-v0.x), (v2.x-v0.x), (v1.y-v0.x), (v2.y-v0.y)}})*Vector2{xx-v0.x, yy-v0.y};
//                auto deb1 = coords.x * (v1.x-v0.x) + coords.y*(v2.x-v0.x) + v0.x - xx;
//                assert(fabs(coords.x * (v1.x-v0.x) + coords.y*(v2.x-v0.x) + v0.x - xx) < 0.01);
//                auto deb2 = coords.x * v1.y + coords.y*v2.y + v0.y - yy;
//                assert(coords.x * v1.y + coords.y*v2.y + v0.y - yy < 0.01);
//                Component newdepth = coords.x*v1.z + coords.y*v2.z + v0.z;

                if((visible && (!neg || !pos))  /*&& fabs(newdepth) <= 1 && newdepth < depth*/) {
//                    depth = newdepth;
                    buffer = qwq;
                }
            }
        };
        auto rend = [&](int32_t m, const ExplicitObject &e) {
            if(e.type == Me) {
                for(auto x = e.mesh.begin(); x < e.mesh.end(); x++) {
                    toRaw(m, *x);
                }
            }
            else {
                abort();
            }

        };
    //    auto frame = [&](HyperEntity h) {
    //        if(h.type == Polygon) {
    //            renderPrimitive(GL_LINE_LOOP,  [&]() {
    //                for(auto x : h.p) {
    //                    transform (x);
    //                }
    //            }
    //            );

    //        }
    //    };
    for(auto& i : scene.ex) {
        rend( i.first, i.second);
    }
    if(showMainAxes) {

        rend(0, {Me, xarrow(0.06, false)});
        rend(0, {Me, yarrow(0.06, false)});
        rend(0, {Me, zarrow(0.06, false)});

    }

    return buffer;
}

//std::ostream& Matrix::operator <<(std::ostream& stream, const SelectedThing & thing) {
//    stream << "SelectedThing {" <<
//}

void mouseMCase (SDL_MouseMotionEvent a) {
    if((a.state & SDL_BUTTON_MIDDLE) != 0) {
        view = rotateAroundY (fromGradi (a.yrel)) * view;
        view = rotateAroundZ (fromGradi (-a.xrel)) * view;
    } else {// a * width + b = 1
           // a * 0 + b = -1
        // b = -1
        // a = 2 / width
        auto iint = selected(double(a.x*2)/width - 1, 1 - double(a.y*2)/height);
        if( iint > 0 ) {
            selectedThing = {Mes, iint};
        } else {
          selectedThing = {Nihil, -1};
//          pfint(get selectedThing) >>= print
//        putStrLn $ "11111111111111111:   " ++ showHex int
        }
    }
}
void mouseCCase(SDL_MouseButtonEvent a) {
    if(state == Ground && a.button == SDL_BUTTON_LEFT) {
        for(int i = 0; i < gui.size(); i++) {
            if (buttRectangle(gui[i],i).contains( a.x, a.y ) ) {
              if(gui[i].active()) {
                  gui[i].action();
              }
              return;
            }
        }
    } else if(state == AddingWall && a.button == SDL_BUTTON_LEFT) {
        int x, y;
        SDL_GetMouseState(&x, &y);

        Mesh mesh = beingAddedWall( 0, {x, y});
        scene.ex[scene.ex.size()] = {Me, mesh};
        state = Ground;
    }
}
void editorLoop() {

  while(true) {
      SDL_Event event;
      if(SDL_PollEvent(&event)) {
        switch(event.type) {
        case SDL_KEYDOWN:{
            keyboardCase(event.key);
        }break;
        case SDL_TEXTEDITING:
        case SDL_TEXTINPUT:{
        }break;
        case SDL_MOUSEMOTION:{
            mouseMCase(event.motion);
        }break;
        case SDL_MOUSEBUTTONDOWN:{
            mouseCCase(event.button);
        }break;
        case SDL_MOUSEWHEEL:{
        }break;
        case SDL_QUIT: return;
        case SDL_CLIPBOARDUPDATE: {
        }break;
        case SDL_DROPFILE:{
        }break;
        default: {
        }
        }
      } else {
          editorDisplay();
      }
    }
}
#endif // EDITOR_H

