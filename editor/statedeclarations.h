#ifndef STATEDECLARATIONS_H
#define STATEDECLARATIONS_H
#include "util/hyperbolic.h"
struct Moving;
struct GroundS;
struct AddingWallS;
struct RotatingCamera;
struct SelectedMesh;
typedef boost::variant<GroundS, Moving,AddingWallS, RotatingCamera, struct Input, SelectedMesh> EditorState;
using namespace H;
struct SDL_MouseMotionEvent;
struct SDL_MouseButtonEvent;
struct GroundS {
    EditorState mouseButtonDown(const SDL_MouseButtonEvent& a) const;
    EditorState mouseMotion(const SDL_MouseMotionEvent& a);
    struct PreSelectedThing {
        SelectedThingType type = Nihil;
        static constexpr const char *const TypeNames[] = {"Nihil", "Mes", "Obs"};
        int32_t n = -1;
    } preselectedThing;

};
struct ExplicitObject;
struct SelectedMesh {
//    EditorState mouseButtonDown(const SDL_MouseButtonEvent& a);
//    EditorState mouseMotion(const SDL_MouseMotionEvent& a);
    struct SelectedThing {
        ExplicitObject* p = 0;
        Matrix44 m = identity;
        Point center = {{0,0,0,0}};
        Component size = 0;
        Vector2 x;
        Vector2 y;
        Vector2 z;
    } selectedThing;

};
struct Moving {
    enum {x, y, z} direction;
    EditorState mouseMotion(const SDL_MouseMotionEvent &a);
    SelectedMesh::SelectedThing selectedThing;
    EditorState mouseButtonUp(const SDL_MouseButtonEvent& event);
};
struct AddingWallS {
    EditorState mouseButtonDown(const SDL_MouseButtonEvent& a);
};
struct RotatingCamera {
    EditorState mouseMotion(const SDL_MouseMotionEvent &a);
    EditorState mouseButtonUp(const SDL_MouseButtonEvent& event);
};
 struct LineEdit;
struct Input {
    LineEdit* le;
};

#endif // STATEDECLARATIONS_H





