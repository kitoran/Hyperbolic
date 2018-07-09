#ifndef EDITOR_H
#define EDITOR_H
#include "SDL2/SDL.h"
#define GL_GLEXT_PROTOTYPES
#include "GL/gl.h"
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
//buttRectangle :: Button -> Rectangle
void editorDisplay(bool selection ) {
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
    auto toRaw = [&transform, selection, &applyNormal](bool bb, int32_t qwq, const ColoredEntity& ce) {
        if(ce.e.type == Polygon) {
            renderPrimitive(GL_POLYGON, [&transform, &applyNormal, selection, bb, qwq, &ce](){
                if(selection) {
                    glColor4bv((GLbyte*)(&qwq));
                } else {
                    if(bb) {
                        glColor4f(clamp(ce.color.r + 0.3), clamp(ce.color.g + 0.3), clamp(ce.color.b + 0.3), clamp(ce.color.a + 0.3));
                    } else {
                        glColor4fv(ce.color.m);
                    }
                    applyNormal(ce.e.p);
                    for(auto x:ce.e.p) {
                        transform(x);
                    }
                }
            });
        } else if(ce.e.type == Segment) {
            renderPrimitive(GL_LINES, [&](){
                glColor4dv((double*)(&(ce.color)));
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
    auto rend = [&](int32_t m, ExplicitObject e) {
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
        glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        for(auto i : scene.ex) {
            rend( i.first, i.second);
        }
        if(showMainAxes) {
//            glEnable(GL_LINE_STIPPLE);
//            glLineStipple(2, 0xAAAA);
//            renderPrimitive(GL_LINES, [&](){
//                glColor4d(1, 0, 0, 1);
//                Point rn = G::persViewMatrix * ( view * Point{-1, 0, 0, 1});
//                Point rp = G::persViewMatrix * ( view * Point{1, 0, 0, 1});
//                glVertex3d(rn.x/rn.t*sign(rn.t)+0.01, rn.y/rn.t*sign(rn.t), -0.999);
//                glVertex3d(rp.x/rp.t*sign(rp.t)+0.01, rp.y/rp.t*sign(rp.t), -0.999);
//                glColor4d(0, 1, 0, 1);
//                Point gn = G::persViewMatrix * ( view * Point{0,-1, 0, 1});
//                Point gp = G::persViewMatrix * ( view * Point{0,1,  0, 1});
//                glVertex4dv(saneVertex4({gn.x/gn.t+0.01, gn.y/gn.t, -0.999,1}).data);
//                glVertex4dv(saneVertex4({gp.x/gp.t+0.01, gp.y/gp.t, -0.999,1}).data);
//                glColor3b(0, 0, 127);
//                Point bn = G::persViewMatrix * ( view * Point{0,0,-1, 1});
//                Point bp = G::persViewMatrix * ( view * Point{0,0,1,  1});
//                glVertex4dv(saneVertex4({bn.x/bn.t+0.01, bn.y/bn.t, -0.999,1}).data);
//                glVertex4dv(saneVertex4({bp.x/bp.t+0.01, bp.y/bp.t, -0.999,1}).data);

//            });
//
            glDisable(GL_DEPTH_TEST);
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
            glEnable(GL_DEPTH_TEST);
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

        }
        if(state == AddingWall) {
            int x, y;
            SDL_GetMouseState(&x, &y);
            for(auto fwe : beingAddedWall(0, {double(x), double(y)})) {
                toRaw(false, -1, fwe);
            }
        }
        glColor4d( 1, 1, (1), 1);
        if(not selection) {

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

    }
//    FOR4(i) {
//        FOR4(j) {
//            std::cout <<  view(i,j) << " ";
//        }
//        std::cout << "\n";
//    }
    std::cout << insanity(view) << " " << insanity(identity) << insanity(moveAlongX (-0.1) * identity)  << std::endl;

}
int32_t selected(int32_t xx, int32_t yy) {
    char res[4];
    GLint viewport[4];

    editorDisplay(true);
    glGetIntegerv( GL_VIEWPORT, viewport);

    /*  -- GL.clearColor $= GL.Color4 (0x44/255) (0x44/255) (0x44/255) (0x44/255)
    -- GL.clear [ColorBuffer]
    -- glReadPixels 1 1  1 1 GL_BGRA GL_UNSIGNED_BYTE res*/
    glReadPixels( xx, ((viewport[3] - yy)), 1, 1, GL_BGRA, GL_BYTE, res);
    //  print ("her 1" ++ show ress)
    //  -- ress2 <- peek res
    //  -- print ("her 2" ++ show ress2)
    //  -- glReadPixels xx (traceComm "v3 - yy" (v3 - yy)) 1 1 GL_BGRA GL_INT res
    //  -- ress3 <- peek res
    //  -- print ("her 3" ++ show ress3)
    return *((int32_t*)(res));
}

//std::ostream& Matrix::operator <<(std::ostream& stream, const SelectedThing & thing) {
//    stream << "SelectedThing {" <<
//}

void mouseMCase (SDL_MouseMotionEvent a) {
    if((a.state & SDL_BUTTON_MIDDLE) != 0) {
        view = rotateAroundY (fromGradi (a.yrel)) * view;
        view = rotateAroundZ (fromGradi (-a.xrel)) * view;
    } else {
        auto iint = selected(a.x, a.y);
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
    if(state == Ground) {
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
          editorDisplay(false);
      }
    }
}
#endif // EDITOR_H

