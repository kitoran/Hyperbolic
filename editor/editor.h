#ifndef EDITOR_H
#define EDITOR_H
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
enum SelectedThingType {Nihil, Mes, Obs};

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


using Angle = double;
Vector2 mapVertexPixel(Vector2 ab);
using namespace G;
boost::optional<Mesh> beingAddedWall(Angle /*a*/, Vector2 pos);
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
void editorLoop();
#endif // EDITOR_H

