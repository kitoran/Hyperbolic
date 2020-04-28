#include "commongraphics.h"

SDL_Window* window = 0;
SDL_GLContext context;
int G::width = 1024, G::height = 760;
namespace G {
bool frame = true;
bool wheCons = false;

void initialiseGraphics(int sg, char **hr) {
    glutInit(&sg, hr);
    SDL_Init(SDL_INIT_VIDEO);

    SDL_DisplayMode display;
    SDL_GetCurrentDisplayMode(0, &display);

    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    width = display.w; height = display.h;
    window = SDL_CreateWindow("Hyperbolic",  0, 0, display.w, display.h, SDL_WINDOW_BORDERLESS | SDL_WINDOW_OPENGL);
    SDL_SetWindowFullscreen(window, SDL_WINDOW_FULLSCREEN_DESKTOP);
    context = SDL_GL_CreateContext(window);

    glDepthFunc( GL_LESS );

    glEnable(GL_DEPTH_TEST);
    glDisable(GL_DEPTH_CLAMP);

    glEnable(GL_LINE_SMOOTH);
    glLineWidth(2);

    glEnable(GL_POLYGON_OFFSET_FILL);
    glPolygonOffset(-0.2, 1);

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
//    auto cursor = SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_CROSSHAIR);
//    auto cursor = SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_NO);
//    SDL_SetCursor(cursor);
    SDL_ShowCursor(SDL_FALSE);
    SDL_ShowWindow(window);


    std::vector<unsigned char> image;
    unsigned width, height;
    unsigned error = lodepng::decode(image, width, height, "/home/n/Hyperbolic/zv2mh98hluu31.png");
    if(error != 0) {
        printf("error %d: %s\n", error, lodepng_error_text(error));
        exit(1);
    }
    glEnable(GL_TEXTURE_2D);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST); //GL_NEAREST = no smoothing
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

    glTexImage2D(GL_TEXTURE_2D, 0, 4/*wtf??*/
                 , width, height,
                 0,
                 GL_RGBA,
                 GL_UNSIGNED_BYTE, &image[0]);

}

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
const Mesh G::sourceMesh() {// -- aaa-aba aba-abb abb-aab aab-aaa
    double r = 0.002;
    Point aaa {r, r, r, 1};
    Point aab {r, r, -r, 1};
    Point aba {r, -r, r, 1};
    Point baa {-r, r, r, 1};
    Point abb {r, -r, -r, 1};
    Point bab {-r, r, -r, 1};
    Point bba {-r, -r, r, 1};
    Point bbb {-r, -r, -r, 1};
    return Mesh {{{0.9, 0.0, 1.0, 1}, {Polygon, {aaa, aab, abb, aba}}},
        {{1.0, 0.0, 0.9, 1}, {Polygon, {baa, bba, bbb, bab}}},
        {{0.9, 0.0, 1.0, 1}, {Polygon, {aaa, aba, bba, baa}}},
        {{0.9, 0.0, 1.0, 1}, {Polygon, {aab, bab, bbb, abb}}},
        {{0.9, 0.0, 1.0, 1}, {Polygon, {aab, aaa, baa, bab}}},
        {{0.9, 0.0, 1.0, 1}, {Polygon, {aba, abb, bbb, bba}}}};
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
