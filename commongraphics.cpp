#include "commongraphics.h"
#include <chrono>
#include <iostream>
#include <fcntl.h>
#include <libexplain/fwrite.h>
#include <libexplain/fopen.h>
#include <libexplain/fread.h>

SDL_Window* window = 0;
SDL_GLContext context;
int G::width = 1024, G::height = 760;
namespace G {
bool frame = true;
bool wheCons = false;

void initialiseGraphics(int /*sg*/, char **/*hr*/) {
//    glutInit(&sg, hr);
    using namespace std::chrono;
    auto start = high_resolution_clock::now();
    SDL_Init(SDL_INIT_VIDEO);
    std::cerr << duration_cast<milliseconds>(high_resolution_clock::now() - start).count() << " ms 1\n" ;
    start = high_resolution_clock::now();

    SDL_DisplayMode display;
    SDL_GetCurrentDisplayMode(0, &display);
    std::cerr << duration_cast<milliseconds>(high_resolution_clock::now() - start).count() << " ms 2\n" ;
    start = high_resolution_clock::now();

    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    width = display.w; height = display.h;
    window = SDL_CreateWindow("Hyperbolic",  0, 0, display.w, display.h, SDL_WINDOW_BORDERLESS | SDL_WINDOW_OPENGL);
    SDL_SetWindowFullscreen(window, SDL_WINDOW_FULLSCREEN_DESKTOP);
    context = SDL_GL_CreateContext(window);

    glDepthFunc( GL_LESS );
    std::cerr << duration_cast<milliseconds>(high_resolution_clock::now() - start).count() << " ms 3\n" ;
    start = high_resolution_clock::now();

    glEnable(GL_DEPTH_TEST);
    glDisable(GL_DEPTH_CLAMP);

    glEnable(GL_LINE_SMOOTH);
    glLineWidth(2);

    glEnable(GL_POLYGON_OFFSET_FILL);
    glPolygonOffset(-0.2, 1);

    std::cerr << duration_cast<milliseconds>(high_resolution_clock::now() - start).count() << " ms 4\n" ;
    start = high_resolution_clock::now();
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
//    auto cursor = SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_CROSSHAIR);
//    auto cursor = SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_NO);
//    SDL_SetCursor(cursor);
    SDL_ShowCursor(SDL_FALSE);
    std::cerr << duration_cast<milliseconds>(high_resolution_clock::now() - start).count() << " ms 5\n" ;
    start = high_resolution_clock::now();
    SDL_ShowWindow(window);
    std::cerr << duration_cast<milliseconds>(high_resolution_clock::now() - start).count() << " ms 6\n" ;
    start = high_resolution_clock::now();


    std::vector<unsigned char> image;
    uint32_t width, height;
    FILE* dwdq = explain_fopen_or_die("/home/n/Hyperbolic/zv2mh98hluu31.bmp", "r");
    explain_fread_or_die(&width, 4, 1, dwdq);
    explain_fread_or_die(&height, 4, 1, dwdq);
    image.resize(width*height*4);
    explain_fread_or_die(image.data(), 1, image.size(), dwdq);

//    unsigned error = lodepng::decode(image, width, height, "/home/n/Hyperbolic/zv2mh98hluu31.png");
//    FILE* dwdq = explain_fopen_or_die("/home/n/Hyperbolic/zv2mh98hluu31.bmp", "w");
//    explain_fwrite_or_die(&width, 4, 1, dwdq);
//    explain_fwrite_or_die(&height, 4, 1, dwdq);
//    explain_fwrite_or_die(image.data(), 1, image.size(), dwdq);
//    exit(0);

    std::cerr << duration_cast<milliseconds>(high_resolution_clock::now() - start).count() << " ms 7\n" ;
    start = high_resolution_clock::now();

//    if(error != 0) {
//        printf("error %d: %s\n", error, lodepng_error_text(error));
//        exit(1);
//    }
    glEnable(GL_TEXTURE_2D);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST); //GL_NEAREST = no smoothing
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

    glTexImage2D(GL_TEXTURE_2D, 0, 4/*wtf??*/
                 , width, height,
                 0,
                 GL_RGBA,
                 GL_UNSIGNED_BYTE, &image[0]);
    std::cerr << duration_cast<milliseconds>(high_resolution_clock::now() - start).count() << " ms 8\n" ;
    start = high_resolution_clock::now();


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
        {{0.0, 1.0, 0.0, 0.5}, {Polygon, {aaa, aba, bba, baa}}},
        {{0.0, 0.0, 1.0, 0.5}, {Polygon, {aab, bab, bbb, abb}}},
        {{0.0, 1.0, 1.0, 0.5}, {Polygon, {aab, aaa, baa, bab}}},
        {{0.0, 0.0, 1.0, 0.5}, {Polygon, {aba, abb, bbb, bba}}}};
    //  where
}

const Mesh G::deviator(double size) {// -- aaa-aba aba-abb abb-aab aab-aaa
    static Mesh res;
    static bool init = false;
    if(init) {
        return res;
    }
    init = true;

    double r = 0.005;
    Point leftInp = moveAlongX(-size)*moveAlongY(r)*origin;
    Point rightInp = moveAlongX(-size)*moveAlongY(-r)*origin;
    Point innerCorner = moveAlongX(-r)*moveAlongY(-r)*origin;
    Point rightOutp = moveAlongX(-r)*moveAlongY(-size)*origin;
    Point leftOutp = moveAlongX(r)*moveAlongY(-size)*origin;
    Point outerCorner = moveAlongX(r)*moveAlongY(r)*origin;
    Point lastPoint = outerCorner;
    std::vector<Point> upperFace;
    std::vector<Point> lowerFace;
    for(const Point& newPoint : {leftInp,
                          rightInp,
                            innerCorner,
                            rightOutp,
                            leftOutp,
                            outerCorner}) {
        Point unp = movePerpendicularlyToOxy(r, newPoint);
        Point lnp = movePerpendicularlyToOxy(-r, newPoint);
        upperFace.push_back(unp);
        lowerFace.push_back(lnp);
        Point ulp = movePerpendicularlyToOxy(r, lastPoint);
        Point llp = movePerpendicularlyToOxy(-r, lastPoint);
        res.push_back({{0.0, 0.0, 1.0, 1}, {Polygon, {unp, ulp, llp, lnp}}});
    }
    res.push_back({{0.0, 0.0, 1.0, 1}, {Polygon, upperFace}});
    res.push_back({{0.0, 0.0, 1.0, 1}, {Polygon, lowerFace}});
    return res;
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
