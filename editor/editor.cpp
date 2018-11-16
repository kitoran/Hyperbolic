#include "boost/variant.hpp"

#include "editor.h"
enum SelectedThingType {Nihil, Mes, Obs};
#include "statedeclarations.h"
#include "SDL2/SDL.h"
//#define GL_GLEXT_PROTOTYPES
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
void keyboardCaseEditor (SDL_KeyboardEvent a);
double fromGradi(double x);
//enum StateEditor { Ground, AddingWall, Input };
enum ExplicitObjectType {Me, So, Re};
struct ExplicitObject {
    ExplicitObjectType type;

       Mesh mesh = {};
       Source source = {};
       Receiver receiver = {};

};
ExplicitObject operator *(Matrix44 m, const ExplicitObject &eo);
struct Scene {
    std::map<int32_t, ExplicitObject> ex;
    std::map<int32_t, Obstacle> im;
};
Scene mmmm();


boost::optional<Mesh> beingAddedWall(double /*a*/, Vector2 pos);
Vector2 mapVertexPixel(Vector2 ab);
using namespace G;
struct Button {
    const char* text;
//    Vector2 _pos;
    bool(*active)();
    EditorState(*action)();
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
void mapPixelVertex(double a, double b);
void displayRectangle(Rectangle r);
const auto margin = 3;
Rectangle buttRectangle (const Button & butt, int number/*text (Pos a b) _ _ _*/);void displayButton (Button butt, int number);
void displayLineedit (const LineEdit& edit);
Mesh xarrow(double sizeo, bool transparent);
Mesh yarrow(double sizeo, bool transparent);
void transform (const Point &p);
Mesh zarrow(double sizeo, bool transparent);

void editorDisplay();

int32_t preselected(double xx, double yy);

//std::ostream& Matrix::operator <<(std::ostream& stream, const SelectedThing & thing) {
//    stream << "SelectedThing {" <<
//}

void mouseMCase (SDL_MouseMotionEvent a);

void mouseCCase(SDL_MouseButtonEvent a);

EditorState stateEditor;
Matrix44 view = /*rotateAroundY (0.1) /* (tau/(4-0.1))* /  * moveAlongX (-0.1) /* moveAlongZ (-0.1) !*! ) -* / */ identity;
std::map<SDL_Scancode, H::Matrix44> matricesMove =
 {{SDL_SCANCODE_W, moveAlongX (-stepEditor)}, {SDL_SCANCODE_S, moveAlongX(stepEditor)},
   {SDL_SCANCODE_A, moveAlongY (-stepEditor)}, {SDL_SCANCODE_D, moveAlongY(stepEditor)},
  {SDL_SCANCODE_Z, moveAlongZ  (-stepEditor)}, {SDL_SCANCODE_C, moveAlongZ (stepEditor)}};
//StateEditor stateEditor;

Scene scene = mmmm();
//LineEdit* selectedLineedit;
bool showMainAxes = true;
template<typename T>
class selector : public boost::static_visitor<bool> {
    public:
    template <typename F>
    bool operator () (const F& ) const {
        return std::is_same<T,F>::value;
    }
};
std::vector<Button> buttons = { {"wall", [](){
            return boost::apply_visitor(selector<GroundS>(), stateEditor);
                                 },
                                 ( []()->EditorState {return AddingWallS();} )},
        {"main axes", [](){return true;}, ( []()->EditorState { showMainAxes = !showMainAxes; return stateEditor;} )}
                              };
std::vector<LineEdit> lineedits = { {"1024", {20, 20, 0, 0}, 0, [](){return true;}, {} } };
struct {
    int x;
    int y;
} dragStart;
bool drag = false;
EditorState GroundS::mouseButtonDown(const SDL_MouseButtonEvent &a) const {
    //        if(a.button == SDL_BUTTON_LEFT) {
    //            dragStart = {a.x, a.y};
    //            drag = true;
    //        }
    if(a.button == SDL_BUTTON_LEFT) {
        for(int i = 0; i < buttons.size(); i++) {
            if (buttRectangle(buttons[i],i).contains( a.x, a.y ) ) {
                if(buttons[i].active()) {
                    return buttons[i].action();
                }
                return *this;
            }
        }
        for(int i = 0; i < lineedits.size(); i++) {
            if (lineedits[i].r.contains( a.x, a.y ) ) {
                if(lineedits[i].active()) {
                    //                    selectedLineedit = &lineedits[i];
                    SDL_StartTextInput();
                    return Input{&lineedits[i]};
                    //                    SDL_SetTextInputRect(selectedLineedit->r);
                    //                    *((int*)0) = 8;
                }
                return*this;
            }
        }
        if(preselectedThing.type == Mes) {
            if(scene.ex.find(preselectedThing.n) == scene.ex.end()) return *this;
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
            SelectedMesh::SelectedThing selectedThing;
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
            return SelectedMesh{selectedThing};
        } else if(a.state & SDL_BUTTON_RMASK) {
            return RotatingCamera();
        } else return *this;
    } else if(a.button == SDL_BUTTON_MIDDLE) {
        return RotatingCamera();
    } else {
        assert(a.button == SDL_BUTTON_RIGHT);
        if(a.state & SDL_BUTTON_LMASK) {
            return RotatingCamera();
        }
        return *this;
    }
}

EditorState GroundS::mouseMotion(const SDL_MouseMotionEvent &a)
{
    auto iint = preselected(double(a.x*2)/width - 1, 1 - double(a.y*2)/height);
    if( iint != 0 && iint != 0xffffffff) {
        preselectedThing = {Mes, iint};
    } else {
        preselectedThing = {Nihil, -1};
        //          pfint(get selectedThing) >>= print
        //        putStrLn $ "11111111111111111:   " ++ showHex int
    }
    return *this;
}

EditorState AddingWallS::mouseButtonDown(const SDL_MouseButtonEvent &a) {
    if(a.button == SDL_BUTTON_LEFT) {
        int x, y;
        SDL_GetMouseState(&x, &y);

        auto baw = beingAddedWall(0, {double(x), double(y)});
        if(baw.is_initialized()) {
            scene.ex[scene.ex.size()] = {Me, boost::get(baw)};
        }
        return GroundS();

    } else return *this;
}

EditorState RotatingCamera::mouseMotion(const SDL_MouseMotionEvent& a)
{
    view = rotateAroundY (fromGradi (a.yrel)) * view;
    view = rotateAroundZ (fromGradi (-a.xrel)) * view;
    return *this;
}

EditorState Moving::mouseMotion(const SDL_MouseMotionEvent &a)
{
    if((a.state & SDL_BUTTON_LMASK) != 0) {
        struct {
            double x;
            double y;
        } mouse = {(double(a.x*2)/width - 1), (1 - double(a.y*2)/height)};
        struct {
            double x;
            double y;
        } mouseOld = {(double(dragStart.x*2)/width - 1), (1 - double(dragStart.y*2)/height)};
        //            struct {
        //                double x;
        //                double y;
        //            } mouseRelC = {(double((a.xrel)*2)/width ), (double((a.yrel)*2)/height)};
        struct {
            double x;
            double y;
        } mouseRelD = {mouse.x - mouseOld.x, mouse.y - mouseOld.y};
        Point sm = /* normalizeWass(*/moveRightTo(selectedThing.center) *
                Point{(direction == x) * 0.9,
                (direction == y) * 0.9,
                (direction == z) * 0.9, 1}/*)*/;
        // sm - это точка, к которой мы двигаем хуйню
        assert(proper(sm));

        Matrix44 m = G::persViewMatrix * view;
        Point s = m*sm;
        Point s1 = m*selectedThing.center;
        double x0 = s.x/s.t;
        double y0 = s.y/s.t;
        //            auto z0 = s.z/s.t;
        //            auto t0 = s.t/s.t;
        double  x1 = s1.x/s1.t;
        double  y1 = s1.y/s1.t;
        //            auto z1 = s1.z/s1.t;
        //            auto t1 = s1.t/s1.t;
        Point dnew;
        Point dold;
        Point* m_0 = (Point*)(&m(0,0));
        Point* m_1 = (Point*)(&m(1,0));
        //            {
        double cax = *m_0 * selectedThing.center;
        double cbx = *m_0 * sm;
        double cay = *m_1 * selectedThing.center;
        double cby = *m_1 * sm;
        // xnew = cax + t * cbx
        // dist = (cax + t * cbx - mouse.x )^2 + (cay + t * cby - mouse.y )^2
        //dist' = 2 cbx (cax + cbx t - mouse.x) + 2 cby (cay + cby t - mouse.y)
        double tnew = (-cax*cbx + cbx*mouse.x - cay*cby + cby*mouse.y)/(cbx*cbx + cby*cby);
        double told = (-cax*cbx + cbx*mouseOld.x - cay*cby + cby*mouseOld.y)/(cbx*cbx + cby*cby);
        glDisable(GL_DEPTH_TEST);
        SDL_GL_SwapWindow(window);
        //            assert((ynew-y1)*mouseRelC.y >= 0);
        //            assert((xnew-x1)*mouseRel.x >= 0);
        //            auto ijkl = tran.m+12;
        //            double  bnew = -(ynew*(*m3*selectedThing.center) - *m_1*selectedThing.center)/((*m_1*sm)*(*m3*selectedThing.center)
        //                                                                                        -(*m_1*selectedThing.center)*(*m3*sm));
        //            double  anew = (ynew - bnew*(*m_1*sm));
        dnew = selectedThing.center + tnew * sm; // invpv * Point{x, y, c, 1};//fabs ( ijkl[2]) > 0.00001 ? Point{ x, y,  c, 1} : Point{ 0, 0, 1, 0});
        bool proper;
        {
            Point max = /* normalizeWass(*/moveRightTo(selectedThing.center) *
                    Point{(direction == x) * 1.0,
                    (direction == y) * 1.0,
                    (direction == z) * 1.0, 1}/*)*/;
            Point min = /* normalizeWass(*/moveRightTo(selectedThing.center) *
                    Point{(direction == x) * (-1.0),
                    (direction == y) * (-1.0),
                    (direction == z) * (-1.0), 1}/*)*/;
            double ymax = (m * max).y;
            double ymin = (m * min).y;
            double  tnew = (x0*y1-x1*y0 - mouse.x*(y1-y0) + mouse.y*(x1-x0))/((y1-y0)*(y1-y0) + (x1-x0)*(x1-x0));

            double xnew = mouse.x + tnew*(y1-y0);
            double ynew =  mouse.y + tnew*(x0-x1);
            proper = (ynew <= ymax && ynew >= ymin) || (ynew >= ymax && ynew <= ymin);

        }
        //            }
        //            {
        //            double  told = (x0*y1-x1*y0 - mouseOld.x*(y1-y0) + mouseOld.y*(x1-x0))/((y1-y0)*(y1-y0) + (x1-x0)*(x1-x0));

        //            double  xold = mouseOld.x + told*(y1-y0);
        //            double  yold = mouseOld.y + told*(x0-x1);
        //            auto ijkl = tran.m+12;
        //            double  bold = -(yold*(*m3*selectedThing.center) - *m_1*selectedThing.center)/((*m_1*sm)*(*m3*selectedThing.center)
        //                                                                                        -(*m_1*selectedThing.center)*(*m3*sm));
        //            double  aold = (yold - bold*(*m_1*sm));
        dold = selectedThing.center + told * sm;//aold*selectedThing.center + bold * sm; // invpv * Point{x, y, c, 1};//fabs ( ijkl[2]) > 0.00001 ? Point{ x, y,  c, 1} : Point{ 0, 0, 1, 0});
        //            }
        //            auto c = b*(m(2, 0)*sm.x + m(2, 1)*sm.y + m(2, 2)*sm.z + m(2, 3)*sm.t)+(m(2, 0)*selectedThing.center.x
        //                                                                                    + m(2, 1)*selectedThing.center.y
        //                                                                                    + m(2, 2)*selectedThing.center.z
        //                                                                                    + m(2, 3)*selectedThing.center.t);
        //            auto invpv =  inv44  (m);
        {
            //                auto ne = m*dnew; auto ol = m*dold;
            //                assert((x0 - x1)*(ne.x/ne.t - ol.x/ol.t) >= 0);
        }
        bool d1 = ::proper(dnew);
        //            assert(::proper(dnew) == proper);
        renderPrimitive(GL_POINTS, [=](){
            glColor3f(1,1,1);
            //               glVertex2f(xnew, ynew);
            if(proper) {
                transform(dnew);
                glColor3f(1,1,0);
                transform(dold);
            } else {
                glColor3f(1,0,0);
                glVertex2f(0, 0);
            }
            //               glVertex2f(xold, yold);
        });
        assert(det44({{sm.x, sm.y, sm.z, sm.t,
                       selectedThing.center.x, selectedThing.center.y, selectedThing.center.z, selectedThing.center.t,
                       dnew.x, dnew.y, dnew.z, dnew.t,
                       0,0,0,1}}
                     ) < 0.01);
        assert(det44({{sm.x, sm.y, sm.z, sm.t,
                       dnew.x, dnew.y, dnew.z, dnew.t,
                       dnew.x, dnew.y, dnew.z, dnew.t,
                       0,0.9,0,1}}
                     ) < 0.01);
        //            auto ddd = (persViewMatrix * (view*selectedThing.center));
        //            assert(fabs(ddd.x/ddd.t - x) < 0.5);

        if(::proper(dnew)){
            //              Vector2 dif = {s.x/s.t - s1.x/s1.t, s.y/s.t - s1.y/s1.t};
            //              Component pro = dif * Vector2{a.xrel, -a.yrel};
            Matrix44 m1 = moveFromTo(dold, dnew, distance(dold, dnew)); //(!!(preselectedThing.n & 0xff000000))?moveAlongX:
            //                auto mmmm = m1*selectedThing.center;
            //                assert(fabs(d.x/d.t - mmmm.x/mmmm.t) < 0.1);
            //                assert(fabs(d.y/d.t - mmmm.y/mmmm.t) < 0.1);
            //                assert(fabs(d.z/d.t - mmmm.z/mmmm.t) < 0.1);
            //                      (!!(preselectedThing.n & 0xff0000))?moveAlongY:
            //                *(selectedThing.p) = m1 * *(selectedThing.p);
            selectedThing.m = m1;

        }
        return *this;
    } else abort();
}

void keyboardCaseEditor(SDL_KeyboardEvent a) {
    SDL_Scancode c = a.keysym.scancode;
    auto f = matricesMove.find(c);
    if (f != matricesMove.end()){
        view = f->second * view; /* 0.1 */;
    }
}

EditorState Moving::mouseButtonUp(const SDL_MouseButtonEvent &/*event*/) {
    *selectedThing.p = selectedThing.m**selectedThing.p;
    selectedThing.m = identity;
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

    return SelectedMesh{selectedThing};
}
boost::optional<Mesh> beingAddedWall(double, Vector2 pos) {
    auto xy =  mapVertexPixel( pos);
    auto tran = G::persViewMatrix * view;
    auto ijkl = tran.m+8;
    auto c = -(ijkl[3]+ijkl[0]*xy[0]+ijkl[1]*xy[1])/ijkl[2];
    auto invpv =  inv44  (tran);
    Point d = invpv * (fabs ( ijkl[2]) > 0.00001 ? Point{ xy[0], xy[1],  c, 1} : Point{ 0, 0, 1, 0});
if(!proper(d)) {
    return boost::none;
}
//    auto res = tran !* d, L.V2 x y)
Mesh wall {{ {0.0, 0.0, 1.0, 1.0}, {Polygon, {{0, 0.01, 0, 1},
                                              {0, (-0.01), 0, 1},
                                              {0, (-0.01), 0.01, 1},
                                              {0, 0.01, 0.01, 1}
                                             }}}};
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
return m;

}
void transform (const Point &p) {
    assert(proper(p));
    std::cout << "x = " << p.x << "y = " << p.y << "z = "  << p.z << "t = "  << p.t << std::endl;
    Point a = G::persViewMatrix * ( view * p);
    glVertex4dv(G::saneVertex4(a).data);
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
    glPointSize(20);
    auto toRaw = [&applyNormal](bool bb, const ColoredEntity& ce) {
        if(ce.e.type == Polygon) {
            renderPrimitive(GL_POLYGON, [&applyNormal, bb, &ce](){

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
                transform(ce.e.p[0]);
                transform(ce.e.p[1]);
            });
        } else {
            renderPrimitive(GL_POINTS, [&](){
                glColor4dv((double*)(&(ce.color)));
                transform(ce.e.p[0]);
            });
        }
    };
    auto rend = [&](int32_t m, const ExplicitObject &e) {
        if(e.type == Me) {
            GroundS * grs = boost::get<GroundS>(&stateEditor);
            if(grs && grs->preselectedThing.type == Mes && grs->preselectedThing.n == m) {
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
    if(boost::get<SelectedMesh>(&stateEditor)){
        SelectedMesh::SelectedThing& selectedThing = boost::get<SelectedMesh>(stateEditor).selectedThing;
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
            rend(0, selectedThing.m*(*selectedThing.p));
        }
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
    if(boost::get<AddingWallS>(&stateEditor)) {
        int x, y;
        SDL_GetMouseState(&x, &y);
        auto baw = beingAddedWall(0, {double(x), double(y)});
        if(baw.is_initialized()) {
            for(auto fwe : boost::get(baw)) {
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
    if(boost::get<GroundS>(&stateEditor)) return -1;
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
    if(boost::get<SelectedMesh>(&stateEditor)) {
        SelectedMesh::SelectedThing &selectedThing = boost::get<SelectedMesh>(stateEditor).selectedThing;
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

double fromGradi(double x) {
    return x / 360*tau*7.0/30.0;
}

ExplicitObject operator *(Matrix44 m, const ExplicitObject &eo) {
    return {
        eo.type,
                m*eo.mesh,
        {},
        {}
    };
}

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

Rectangle buttRectangle(const Button &butt, int number) {
    double i = glutBitmapLength(GLUT_BITMAP_TIMES_ROMAN_24, (const unsigned char*)butt.text);
    double h = glutBitmapHeight(GLUT_BITMAP_TIMES_ROMAN_24);
    double x  = 300;
    double y = 200 + number*h+2*margin*number;
    return Rectangle{(x), (y), (i+2*margin), ((h + 2*margin))};
}

void displayLineedit(const LineEdit &edit) {
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
                        {{1,1,1,transparent?0.3f:1}, {Segment, {origin, pp}}},
                            {{1,1,1,transparent?0.3f:1}, {Segment, {origin, pn}}},
                                {{1,1,1,transparent?0.3f:1}, {Segment, {origin, nn}}},
                                    {{1,1,1,transparent?0.3f:1}, {Segment, {origin, np}}}};
}

Mesh zarrow(double sizeo, bool transparent) {
    double size = sizeo / 10;
    Point pp = {size/2,  size/2,-size, 1};
    Point pn = {size/2,  -size/2,-size, 1};
    Point nn = {-size/2,  -size/2,-size, 1};
    Point np = {-size/2,  size/2,-size, 1};
    return moveAlongZ(sizeo/2) * Mesh{{{0,0,1, transparent?0.3f:1},
                {Polygon, {origin, pp, pn}}},
            {{0,0,1, transparent?0.3f:1}, {Polygon, {origin, pn, nn}}},
            {{0,0,1, transparent?0.3f:1}, {Polygon, {origin, nn, np}}},
            {{0,0,1, transparent?0.3f:1}, {Polygon, {origin, np, pp}}},
            {{0,0,0,transparent?0.3f:1}, {Segment, {origin, pp}}},
            {{0,0,0,transparent?0.3f:1}, {Segment, {origin, pn}}},
            {{0,0,0,transparent?0.3f:1}, {Segment, {origin, nn}}},
            {{0,0,0,transparent?0.3f:1}, {Segment, { origin, np}}}};
}
void mouseUpCase(const SDL_MouseButtonEvent event){
    if(event.button != SDL_BUTTON_LEFT) return;

}
class TextEditing : public boost::static_visitor<> {
public:
    TextEditing (SDL_Event *e) : event(e){}
    void operator()(const Input& i) {
        strcpy(i.le->text, event->edit.text);
        i.le->cursor = event->edit.start;
        //                selectedLineedit->
        abort();
    }
    template <typename T> void operator()(const T&) const {
       return;
    }
    SDL_Event* event;
};
template <typename T>
struct sfinae {
    typedef EditorState type;
};
#define VISITOR(a, b) \
class V ## a : public boost::static_visitor<EditorState> {    \
public: \
    V ## a (b *e) : event(e){}    \
    template<typename T>    \
    typename sfinae<decltype(&T:: a )>::type    \
            operator()(T& i) const {    \
        return i. a (*event);   \
    }   \
    template <typename T> EditorState operator()(const T& i) const {    \
       return i;    \
    }   \
    b * event;  \
}

VISITOR(mouseMotion, SDL_MouseMotionEvent);
VISITOR(mouseButtonDown, SDL_MouseButtonEvent);
void editorLoop() {
    SDL_StopTextInput();
    for(auto &l : lineedits) {
        l.r.sx = glutBitmapLength(GLUT_BITMAP_TIMES_ROMAN_24, (const unsigned char*)l.placeholderText);
        l.r.sy = glutBitmapHeight(GLUT_BITMAP_TIMES_ROMAN_24);
    }
    decltype(&GroundS::mouseButtonDown) aaa;

    while(true) {
        SDL_Event event;
        while(SDL_PollEvent(&event)) {
            switch(event.type) {
            case SDL_KEYDOWN:{
                keyboardCaseEditor(event.key);
            }break;
            case SDL_TEXTEDITING: {
                boost::apply_visitor(TextEditing(&event), stateEditor);
            } break;
            case SDL_TEXTINPUT:{
                abort();
            }break;
            case SDL_MOUSEMOTION:{
                stateEditor = boost::apply_visitor(VmouseMotion(&event.motion), stateEditor);
            }break;
            case SDL_MOUSEBUTTONDOWN:{
                stateEditor = boost::apply_visitor(VmouseButtonDown(&event.button), stateEditor);
            }break;
            case SDL_MOUSEBUTTONUP:{
                mouseUpCase(event.button);
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
        }// else {
        editorDisplay();
        //}
        //        SDL_FlushEvents(SDL_QUIT+1, SDL_LASTEVENT);
    }

}

                                                                Vector2 mapVertexPixel(Vector2 ab) {
                                                                    return {((ab[0])*2/(G::width) - 1 ),
                                                                                (1 - (ab[1])*2/(G::height) )};
                                                                }

                                                                void displayButton(Button butt, int number) {
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
                                                                                        {{0,0,0,transparent?0.3f:1}, {Segment, {origin, pp}}},
                                                                                            {{0,0,0,transparent?0.3f:1}, {Segment, {origin, pn}}},
                                                                                                {{0,0,0,transparent?0.3f:1}, {Segment, {origin, nn}}},
                                                                                                    {{0,0,0,transparent?0.3f:1}, {Segment, {origin, np}}}};
                                                                                                }
