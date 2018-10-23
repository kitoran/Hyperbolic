#ifndef EDITOR_H
#define EDITOR_H
#include "SDL2/SDL.h"
#define GL_GLEXT_PROTOTYPES
#include "GL/gl.h"
#include "float.h"
#include "assert.h"
#include "util/hyperbolic.h"
#include "commongraphics.h"
#include "util/physics.h"
#include <map>
#include <set>
#include <iostream>
#undef NULL
#define NULL ((void*)0)
const double stepEditor = 0.1;
using namespace H;
Matrix44 view = /*rotateAroundY (0.1) /* (tau/(4-0.1))* /  * moveAlongX (-0.1) /* moveAlongZ (-0.1) !*! ) -* / */ identity;
std::map<SDL_Scancode, H::Matrix44> matricesMove =
 {{SDL_SCANCODE_W, moveAlongX (-stepEditor)}, {SDL_SCANCODE_S, moveAlongX(stepEditor)},
   {SDL_SCANCODE_A, moveAlongY (-stepEditor)}, {SDL_SCANCODE_D, moveAlongY(stepEditor)},
  {SDL_SCANCODE_Z, moveAlongZ  (-stepEditor)}, {SDL_SCANCODE_C, moveAlongZ (stepEditor)}};

void keyboardCaseEditor (SDL_KeyboardEvent a) {
    SDL_Scancode c = a.keysym.scancode;
    auto f = matricesMove.find(c);
    if (f != matricesMove.end()){
      view = f->second * view; /* 0.1 */;
    }
}
auto fromGradi(double x) {
    return x / 360*tau*7.0/30.0;
}
enum SelectedThingType {Nihil, Mes, Obs};
struct PreSelectedThing {
    SelectedThingType type = Nihil;
    static constexpr const char *const TypeNames[] = {"Nihil", "Mes", "Obs"};
    int32_t n = -1;
} preselectedThing;
enum ExplicitObjectType {Me, So, Re};
struct ExplicitObject {
    ExplicitObjectType type;

       Mesh mesh = {};
       Source source = {};
       Receiver receiver = {};

};
ExplicitObject operator *(Matrix44 m, const ExplicitObject &eo) {
    return {
       eo.type,
        m*eo.mesh,
        {},
       {}
    };
}
struct SelectedThing {
    ExplicitObject* p = 0;
    Point center = {{0,0,0,0}};
    Component size = 0;
    Vector2 x;
    Vector2 y;
    Vector2 z;
} selectedThing;

struct Scene {
    std::map<int32_t, ExplicitObject> ex;
    std::map<int32_t, Obstacle> im;
};
Scene mmmm() {
    std::vector<H::Point> v;
    for( double i : {0, 1, 2, 3} ) {
        auto ni = i*tau;
        auto bbi = ni/4;
        Point p = rotateAroundZ(bbi-0.1) * /*moveAlongZ(-0.1) */ Point {0.1, 0, 0, 1};
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
enum StateEditor { Ground, AddingWall, Input } stateEditor;
Scene scene = mmmm();
using Angle = double;
Vector2 mapVertexPixel(Vector2 ab) {
    return {((ab[0])*2/(G::width) - 1 ),
                                        (1 - (ab[1])*2/(G::height) )};
}
using namespace G;
struct OptionalMesh {
    bool there;
    Mesh m;
};
OptionalMesh beingAddedWall(Angle /*a*/, Vector2 pos) {
    auto xy =  mapVertexPixel( pos);
    auto tran = G::persViewMatrix * view;
    auto ijkl = tran.m+8;
    auto c = -(ijkl[3]+ijkl[0]*xy[0]+ijkl[1]*xy[1])/ijkl[2];
    auto invpv =  inv44  (tran);
    Point d = invpv * (fabs ( ijkl[2]) > 0.00001 ? Point{ xy[0], xy[1],  c, 1} : Point{ 0, 0, 1, 0});
    if(!proper(d)) {
        return {false, {}};
    }
//    auto res = tran !* d, L.V2 x y)
    Mesh wall {{ {0.0, 0.0, 1.0, 1.0}, {Polygon, {{0, 0.01, 0, 1},
                                                     {0, (-0.01), 0, 1},
                                                     {0, (-0.01), 0.01, 1},
                                                     {0, 0.01, 0.01, 1}
                                                       }, origin, origin}}};
//    assert (proper(0,abs (rz) < 0.04) ;
    for(ColoredEntity& a : wall) {
        if(a.e.type == Polygon) {
            for(Point& d : a.e.p) {
                assert (proper(d));
            }
        }
    }
    Mesh m = H::moveRightTo (d) * wall;
    for(ColoredEntity& a : m) {
        if(a.e.type == Polygon) {
            for(Point& d : a.e.p) {
                assert (proper(d));
            }
        }
    }
    return {true, m};

}
struct Button {
    const char* text;
//    Vector2 _pos;
    bool(*active)();
    void(*action)();
};
struct Rectangle {
    double lx;
    double ly;
    double sx;
    double sy;
    bool contains(double x, double y) {
        return lx <= x && ly <= y && (lx+sx) >= x && (ly+sy) >= y;
    }
};
struct LineEdit {
    const char* placeholderText;
    Rectangle r;
    int cursor;
    bool(*active)();
    char text[100];
};
bool showMainAxes = true;
std::vector<Button> buttons = { {"wall", [](){return stateEditor == Ground;}, ( [](){stateEditor = AddingWall;} )},
        {"main axes", [](){return true;}, ( [](){ showMainAxes = !showMainAxes;} )}};
std::vector<LineEdit> lineedits = { {"1024", {20, 20, 0, 0}, 0, [](){return true;}, {} } };
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
}void displayButton (Button butt, int number) {
    double h = glutBitmapHeight(GLUT_BITMAP_TIMES_ROMAN_24);
    double x  = 300;
    double y = 200 + number*h+2*margin*number;
    glColor3f(.3, .3, .3);
    displayRectangle(buttRectangle(butt, number));
    if(butt.active()) {
        glColor3f(1, 1, 1);
    } else {
        glColor3f(.7, .7, .7);
    }
    auto debug = x + margin;
    glWindowPos2f(x + margin, height - (y + h + margin));
    glColor3f(0, 1, 0);
    glutBitmapString(GLUT_BITMAP_TIMES_ROMAN_24, (const unsigned char*)butt.text);
}
void displayLineedit (const LineEdit& edit) {
    double h = glutBitmapHeight(GLUT_BITMAP_TIMES_ROMAN_24);
//    double x  = 300;
//    double y = 200;
    glColor4f(.3, .3, .3, .5);
    displayRectangle(edit.r);
    if(edit.active()) {
        glColor3f(1, 1, 1);
    } else {
        glColor3f(.7, .7, .7);
    }
//    auto debug = x + margin;
    glWindowPos2f(edit.r.lx + margin, height - (edit.r.ly + h + margin));
    glColor3f(1, 1, 1);
    glutBitmapString(GLUT_BITMAP_TIMES_ROMAN_24, (const unsigned char*)edit.text);
}
Mesh xarrow(double sizeo, bool transparent) {
    double size = sizeo / 10;
    Point pp = {-size, size/2, size/2, 1};
    Point pn = {-size, size/2, -size/2, 1};
    Point nn = {-size, -size/2, -size/2, 1};
    Point np = {-size, -size/2, size/2, 1};
    return moveAlongX(sizeo/2) * Mesh{{{1, 0, 0, transparent?0.3f:1}, {Polygon, {origin, pp, pn}}},
                                {{1, 0, 0, transparent?0.3f:1}, {Polygon, {origin, pn, nn}}},
                                {{1, 0, 0, transparent?0.3f:1}, {Polygon, {origin, nn, np}}},
                                {{1, 0, 0, transparent?0.3f:1}, {Polygon, {origin, np, pp}}},
                        {{1,1,1,transparent?0.3f:1}, {Segment, {}, origin, pp}},
                            {{1,1,1,transparent?0.3f:1}, {Segment, {}, origin, pn}},
                                {{1,1,1,transparent?0.3f:1}, {Segment, {}, origin, nn}},
                                    {{1,1,1,transparent?0.3f:1}, {Segment, {}, origin, np}}};
}
Mesh yarrow(double sizeo, bool transparent) {
    double size = sizeo / 10;
    Point pp = { size/2,-size, size/2, 1};
    Point pn = {size/2, -size, -size/2, 1};
    Point nn = {-size/2, -size, -size/2, 1};
    Point np = {-size/2, -size, size/2, 1};
    return moveAlongY(sizeo/2) * Mesh{{{0,1,  0, transparent?0.3f:1}, {Polygon, {origin, pp, pn}}},
                                {{0,1,  0, transparent?0.3f:1}, {Polygon, {origin, pn, nn}}},
                                {{0,1,  0, transparent?0.3f:1}, {Polygon, {origin, nn, np}}},
                                {{0,1,  0, transparent?0.3f:1}, {Polygon, {origin, np, pp}}},
                        {{0,0,0,transparent?0.3f:1}, {Segment, {}, origin, pp}},
                            {{0,0,0,transparent?0.3f:1}, {Segment, {}, origin, pn}},
                                {{0,0,0,transparent?0.3f:1}, {Segment, {}, origin, nn}},
                                    {{0,0,0,transparent?0.3f:1}, {Segment, {}, origin, np}}};
}

Mesh zarrow(double sizeo, bool transparent) {
    double size = sizeo / 10;
    Point pp = {size/2,  size/2,-size, 1};
    Point pn = {size/2,  -size/2,-size, 1};
    Point nn = {-size/2,  -size/2,-size, 1};
    Point np = {-size/2,  size/2,-size, 1};
    return moveAlongZ(sizeo/2) * Mesh{{{0,0,1, transparent?0.3f:1}, {Polygon, {origin, pp, pn}}},
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
        assert(proper(p));
        std::cout << "x = " << p.x << "y = " << p.y << "z = "  << p.z << "t = "  << p.t << std::endl;
        Point a = G::persViewMatrix * ( view * p);
        glVertex4dv(G::saneVertex4(a).data);
    };
    glPointSize(20);
    auto toRaw = [&transform, &applyNormal](bool bb, const ColoredEntity& ce) {
        if(ce.e.type == Polygon) {
            renderPrimitive(GL_POLYGON, [&transform, &applyNormal, bb, &ce](){

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
//            glDisable(GL_DEPTH_TEST);
//            renderPrimitive(GL_POINTS, [&transform, &applyNormal, bb, qwq, &ce](){
//                glColor4f(1, 1, 1, 1);
//                for(auto x:ce.e.p) {
//                    transform(x);
//                }
//            });
//            glEnable(GL_DEPTH_TEST);

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
            if(preselectedThing.type == Mes && preselectedThing.n == m) {
                for(auto x = e.mesh.begin(); x < e.mesh.end(); x++) {
                    toRaw(true, *x);
                }
            } else {
                for(auto x = e.mesh.begin(); x < e.mesh.end(); x++) {
                    toRaw(false, *x);
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
        if(selectedThing.p) {
            Matrix44 mot = moveRightTo(selectedThing.center);
            rend(0, mot * ExplicitObject{Me, xarrow(selectedThing.size, false)});
            rend(0, mot * ExplicitObject{Me, yarrow(selectedThing.size, false)});
            rend(0, mot * ExplicitObject{Me, zarrow(selectedThing.size, false)});
            glDisable(GL_DEPTH_TEST);
            rend(0, mot * ExplicitObject{Me, xarrow(selectedThing.size, true)});
            rend(0, mot * ExplicitObject{Me, yarrow(selectedThing.size, true)});
            rend(0, mot * ExplicitObject{Me, zarrow(selectedThing.size, true)});
            glEnable(GL_DEPTH_TEST);
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
            rend(0, {Me, xarrow(2, false)});
            rend(0, {Me, yarrow(2, false)});
            rend(0, {Me, zarrow(2, false)});
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
            rend(0, {Me, xarrow(2, true)});
            rend(0, {Me, yarrow(2, true)});
            rend(0, {Me, zarrow(2, true)});

        }
        if(stateEditor == AddingWall) {
            int x, y;
            SDL_GetMouseState(&x, &y);
            auto baw = beingAddedWall(0, {double(x), double(y)});
            if(baw.there) {
                for(auto fwe : baw.m) {
                    toRaw(false, fwe);
                }
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

        for(int i = 0; i < buttons.size(); i++) {
            displayButton(buttons[i], i);
        }
        for(int i = 0; i < lineedits.size(); i++) {
            displayLineedit(lineedits[i]);
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

int32_t preselected(double xx, double yy) {
    if(stateEditor != Ground) return -1;
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
    if(selectedThing.p != 0) {
        Matrix44 mot = moveRightTo(selectedThing.center);
        rend(0xff0000ff, mot * ExplicitObject{Me, xarrow(selectedThing.size, false)});
        if(buffer == 0xff0000ff) {

         std::cout << "dgerg";
        }
        rend(0x00ff00ff, mot * ExplicitObject{Me, yarrow(selectedThing.size, false)});
        rend(0x0000ffff, mot * ExplicitObject{Me, zarrow(selectedThing.size, false)});
    }

    return buffer;
}

//std::ostream& Matrix::operator <<(std::ostream& stream, const SelectedThing & thing) {
//    stream << "SelectedThing {" <<
//}

void mouseMCase (SDL_MouseMotionEvent a) {
    if((a.state & SDL_BUTTON_MMASK) != 0) {
        view = rotateAroundY (fromGradi (a.yrel)) * view;
        view = rotateAroundZ (fromGradi (-a.xrel)) * view;
    } else if((a.state & SDL_BUTTON_LMASK) != 0) {
        if(preselectedThing.n == 0xff0000ff || preselectedThing.n == 0x0000ffff || preselectedThing.n == 0x00ff00ff) {
            auto sm = /* normalizeWass(*/moveRightTo(selectedThing.center) *
                    Point{(!!(preselectedThing.n & 0xff000000)) * 0.9,
                        (!!(preselectedThing.n & 0xff0000)) * 0.9,
                    (!!(preselectedThing.n & 0xff00)) * 0.9, 1}/*)*/;
            assert(proper(sm));

            auto m = G::persViewMatrix * view;
            auto s = m*sm;
            auto s1 = m*selectedThing.center;
            auto x0 = s.x/s.t;
            auto y0 = s.y/s.t;
//            auto z0 = s.z/s.t;
//            auto t0 = s.t/s.t;
            auto x1 = s1.x/s1.t;
            auto y1 = s1.y/s1.t;
//            auto z1 = s1.z/s1.t;
//            auto t1 = s1.t/s1.t;
            Point dnew;
            Point dold;
            Point* m3 = (Point*)(&m(3,0));
            Point* m_1 = (Point*)(&m(1,0));
//            {
                auto tnew = (x0*y1-x1*y0 - (double(a.x*2)/width - 1)*(y1-y0) + (1 - double(a.y*2)/height)*(x1-x0))/((y1-y0)*(y1-y0) + (x1-x0)*(x1-x0));

                auto xnew = double(a.x*2)/width - 1 + tnew*(y1-y0);
                auto ynew =  1 - double(a.y*2)/height + tnew*(x0-x1);
//            auto ijkl = tran.m+12;
                auto bnew = -(ynew*(*m3*selectedThing.center) - *m_1*selectedThing.center)/((*m_1*sm)*(*m3*selectedThing.center)
                                                                -(*m_1*selectedThing.center)*(*m3*sm));
                auto anew = (ynew - bnew*(*m_1*sm));
                dnew = anew*selectedThing.center + bnew * sm; // invpv * Point{x, y, c, 1};//fabs ( ijkl[2]) > 0.00001 ? Point{ x, y,  c, 1} : Point{ 0, 0, 1, 0});
//            }
//            {
                auto told = (x0*y1-x1*y0 - (double((a.x-a.xrel)*2)/width - 1)*(y1-y0) + (1 - double((a.y-a.yrel)*2)/height)*(x1-x0))/((y1-y0)*(y1-y0) + (x1-x0)*(x1-x0));

                auto xold = double((a.x-a.xrel)*2)/width - 1 + told*(y1-y0);
                auto yold =  1 - double((a.y-a.yrel)*2)/height + told*(x0-x1);
//            auto ijkl = tran.m+12;
                auto bold = -(yold*(*m3*selectedThing.center) - *m_1*selectedThing.center)/((*m_1*sm)*(*m3*selectedThing.center)
                                                                                -(*m_1*selectedThing.center)*(*m3*sm));
                auto aold = (yold - bold*(*m_1*sm));
                dold = aold*selectedThing.center + bold * sm; // invpv * Point{x, y, c, 1};//fabs ( ijkl[2]) > 0.00001 ? Point{ x, y,  c, 1} : Point{ 0, 0, 1, 0});
//            }
//            auto c = b*(m(2, 0)*sm.x + m(2, 1)*sm.y + m(2, 2)*sm.z + m(2, 3)*sm.t)+(m(2, 0)*selectedThing.center.x
//                                                                                    + m(2, 1)*selectedThing.center.y
//                                                                                    + m(2, 2)*selectedThing.center.z
//                                                                                    + m(2, 3)*selectedThing.center.t);
//            auto invpv =  inv44  (m);
            assert(det44({{sm.x, sm.y, sm.z, sm.t,
                           selectedThing.center.x, selectedThing.center.y, selectedThing.center.z, selectedThing.center.t,
                           dnew.x, dnew.y, dnew.z, dnew.t,
                           0,0,0,1}}
                           ) < 0.1);
            assert(det44({{sm.x, sm.y, sm.z, sm.t,
                           dnew.x, dnew.y, dnew.z, dnew.t,
                           dnew.x, dnew.y, dnew.z, dnew.t,
                           0,0.9,0,1}}
                           ) < 0.1);
//            auto ddd = (persViewMatrix * (view*selectedThing.center));
//            assert(fabs(ddd.x/ddd.t - x) < 0.5);

            if(proper(dnew)){
//              Vector2 dif = {s.x/s.t - s1.x/s1.t, s.y/s.t - s1.y/s1.t};
//              Component pro = dif * Vector2{a.xrel, -a.yrel};
                auto m1 = moveFromTo(dold, dnew, distance(dold, dnew)); //(!!(preselectedThing.n & 0xff000000))?moveAlongX:
//                auto mmmm = m1*selectedThing.center;
//                assert(fabs(d.x/d.t - mmmm.x/mmmm.t) < 0.1);
//                assert(fabs(d.y/d.t - mmmm.y/mmmm.t) < 0.1);
//                assert(fabs(d.z/d.t - mmmm.z/mmmm.t) < 0.1);
//                      (!!(preselectedThing.n & 0xff0000))?moveAlongY:
                *(selectedThing.p) = m1 * *(selectedThing.p);
//                                                        moveAlongZ)(pro/10000);
                {

                    std::set<Point> set;
                    for(const ColoredEntity& ce : (selectedThing.p)->mesh) {
                        if(ce.e.type == Polygon) {
                            for(const Point& e : ce.e.p) {
                                set.insert(normalizeWass(e));
                            }
                        }
                    }
                    auto x = std::minmax_element(set.begin(), set.end(), [](Point a, Point b) {
                        return a.x/a.t < b.x/b.t;
                    });
                    auto y = std::minmax_element(set.begin(), set.end(), [](Point a, Point b) {
                        return a.y/a.t < b.y/b.t;
                    });
                    auto z = std::minmax_element(set.begin(), set.end(), [](Point a, Point b) {
                        return a.z/a.t < b.z/b.t;
                    });
                    selectedThing.center = normalizeWass({(x.second->x/x.second->t + x.first->x/x.first->t)/2,
                                            (y.second->y/y.second->t + y.first->y/y.first->t)/2,
                                            (z.second->z/z.second->t + z.first->z/z.first->t)/2,
                                           1});
                }
//                (selectedThing.center) = m1 * (selectedThing.center);
//                auto ddd = m*selectedThing.center;
//                assert(fabs(ddd.x/ddd.t - x) < 0.5);
            } else {

            }
        }
    } else {// a * width + b = 1
           // a * 0 + b = -1
        // b = -1
        // a = 2 / width
        auto iint = preselected(double(a.x*2)/width - 1, 1 - double(a.y*2)/height);
        if( iint != 0 && iint != 0xffffffff) {
            preselectedThing = {Mes, iint};
        } else {
          preselectedThing = {Nihil, -1};
//          pfint(get selectedThing) >>= print
//        putStrLn $ "11111111111111111:   " ++ showHex int
        }
    }
}

LineEdit* selectedLineedit;
void mouseCCase(SDL_MouseButtonEvent a) {
    if(stateEditor == Ground && a.button == SDL_BUTTON_LEFT) {
        for(int i = 0; i < buttons.size(); i++) {
            if (buttRectangle(buttons[i],i).contains( a.x, a.y ) ) {
              if(buttons[i].active()) {
                  buttons[i].action();
              }
              return;
            }
        }
        for(int i = 0; i < lineedits.size(); i++) {
            if (lineedits[i].r.contains( a.x, a.y ) ) {
                if(lineedits[i].active()) {
                    selectedLineedit = &lineedits[i];
                    stateEditor = Input;
                    SDL_StartTextInput();
//                    SDL_SetTextInputRect(selectedLineedit->r);
//                    *((int*)0) = 8;
                }
                return;
            }
        }
        if(preselectedThing.type == Mes) {
            if(scene.ex.find(preselectedThing.n) == scene.ex.end()) return;
            ExplicitObject*p = &scene.ex[preselectedThing.n];
            std::set<Point> set;
            for(ColoredEntity ce : p->mesh) {
              if(ce.e.type == Polygon) {
                 for(Point e : ce.e.p) {
                     Point q = persViewMatrix*(view*e);
                     assert(proper(e));

                     assert(fabs(q.y/q.t) < 1.01);
                    set.insert(normalizeWass(e));
                 }
              }
            }
            auto x = std::minmax_element(set.begin(), set.end(), [](Point a, Point b) {
                return a.x/a.t < b.x/b.t;
            });
            auto y = std::minmax_element(set.begin(), set.end(), [](Point a, Point b) {
                return a.y/a.t < b.y/b.t;
            });
            auto z = std::minmax_element(set.begin(), set.end(), [](Point a, Point b) {
                return a.z/a.t < b.z/b.t;
            });
            Component size = std::max({x.second->x/x.second->t - x.first->x/x.first->t,
                                       y.second->y/y.second->t - y.first->y/y.first->t,
                                       z.second->z/z.second->t - z.first->z/z.first->t});
            selectedThing.center = normalizeWass({(x.second->x/x.second->t + x.first->x/x.first->t)/2,
                                    (y.second->y/y.second->t + y.first->y/y.first->t)/2,
                                    (z.second->z/z.second->t + z.first->z/z.first->t)/2,
                                   1});
            assert(proper(selectedThing.center));
//            auto yhigh = persViewMatrix*(view**y.second);
//            auto ylow = persViewMatrix*(view**y.first);
//            auto dd = persViewMatrix*(view*selectedThing.center);
            selectedThing.size = size;
            selectedThing.p = p;
        }
    } else if(stateEditor == AddingWall && a.button == SDL_BUTTON_LEFT) {
        int x, y;
        SDL_GetMouseState(&x, &y);

        auto baw = beingAddedWall(0, {double(x), double(y)});
        if(baw.there) {
            scene.ex[scene.ex.size()] = {Me, baw.m};
        }
        stateEditor = Ground;
    }
}
void editorLoop() {
    for(auto &l : lineedits) {
        l.r.sx = glutBitmapLength(GLUT_BITMAP_TIMES_ROMAN_24, (const unsigned char*)l.placeholderText);
        l.r.sy = glutBitmapHeight(GLUT_BITMAP_TIMES_ROMAN_24);
    }
    while(true) {
        SDL_Event event;
        if(SDL_PollEvent(&event)) {
            switch(event.type) {
            case SDL_KEYDOWN:{
                keyboardCaseEditor(event.key);
            }break;
            case SDL_TEXTEDITING: {
                strcpy(selectedLineedit->text, event.edit.text);
                selectedLineedit->cursor = event.edit.start;
//                selectedLineedit->
                abort();
            } break;
            case SDL_TEXTINPUT:{
                                abort();
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
        SDL_FlushEvents(SDL_QUIT+1, SDL_LASTEVENT);
    }

}
#endif // EDITOR_H

