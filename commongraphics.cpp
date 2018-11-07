#include "commongraphics.h"

SDL_Window* window = 0;
SDL_GLContext context;
int G::width = 1024, G::height = 760;
namespace G {
bool frame = true;
bool wheCons = false;
}
const Mesh G::transparentDeviator() {// -- aaa-aba aba-abb abb-aab aab-aaa
    double r = 0.005;
    Point aaa {r, r, r, 1};
    Point aab {r, r, -r, 1};
    Point aba {r, -r, r, 1};
    Point baa {-r, r, r, 1};
    Point abb {r, -r, -r, 1};
    Point bab {-r, r, -r, 1};
    Point bba {-r, -r, r, 1};
    Point bbb {-r, -r, -r, 1};
    return Mesh {{{0.0, 0.0, 1.0, 0.5}, {Polygon, {aaa, aab, abb, aba}}},
        {{1.0, 0.7, 0.0, 0.5}, {Polygon, {baa, bba, bbb, bab}}},
        {{0.0, 0.0, 1.0, 0.5}, {Polygon, {aaa, aba, bba, baa}}},
        {{0.0, 0.0, 1.0, 0.5}, {Polygon, {aab, bab, bbb, abb}}},
        {{0.0, 1.0, 1.0, 0.5}, {Polygon, {aab, aaa, baa, bab}}},
        {{0.0, 0.0, 1.0, 0.5}, {Polygon, {aba, abb, bbb, bba}}}};
    //  where
}

const Mesh G::deviator() {// -- aaa-aba aba-abb abb-aab aab-aaa
    double r = 0.005;
    Point aaa {r, r, r, 1};
    Point aab {r, r, -r, 1};
    Point aba {r, -r, r, 1};
    Point baa {-r, r, r, 1};
    Point abb {r, -r, -r, 1};
    Point bab {-r, r, -r, 1};
    Point bba {-r, -r, r, 1};
    Point bbb {-r, -r, -r, 1};
    return Mesh {{{0.0, 0.0, 1.0, 1}, {Polygon, {aaa, aab, abb, aba}}},
        {{1.0, 0.7, 0.0, 1}, {Polygon, {baa, bba, bbb, bab}}},
        {{0.0, 0.0, 1.0, 1}, {Polygon, {aaa, aba, bba, baa}}},
        {{0.0, 0.0, 1.0, 1}, {Polygon, {aab, bab, bbb, abb}}},
        {{0.0, 1.0, 1.0, 1}, {Polygon, {aab, aaa, baa, bab}}},
        {{0.0, 0.0, 1.0, 1}, {Polygon, {aba, abb, bbb, bba}}}};
    //  where
}

void G::lightenABit(Mesh *d) {
    //    Mesh res(d.size());
    for(int i = 0; i < d->size(); i++) {
        //        ColoredEntity e = d[i];
        (*d)[i].color.r = clamp((*d)[i].color.r+0.3);
        (*d)[i].color.g = clamp((*d)[i].color.g+0.3);
        (*d)[i].color.b = clamp((*d)[i].color.b+0.3);
        //        res[i] = e;
    }
    //    return res;
}
