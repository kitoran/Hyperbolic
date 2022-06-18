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
//void glGenFramebuffers( 	GLsizei n,
//    GLuint *ids);
//void glGenBuffers(	GLsizei 	n,
//    GLuint * 	buffers);
extern "C" void GLAPIENTRY
MessageCallback( GLenum source,
                 GLenum type,
                 GLuint id,
                 GLenum severity,
                 GLsizei length,
                 const GLchar* message,
                 const void* userParam )
{
  fprintf( stderr, "GL CALLBACK: %s type = 0x%x, severity = 0x%x, message = %s\n",
           ( type == GL_DEBUG_TYPE_ERROR ? "** GL ERROR **" : "" ),
            type, severity, message );
}
namespace G {
bool frame = true;
bool wheCons = false;
//unsigned int selectionBuffer;
void initialiseGraphics(int /*sg*/, char **/*hr*/) {
//    glutInit(&sg, hr);\

    SDL_Init(SDL_INIT_VIDEO);

    bool fullscreen = true;

    using namespace std::chrono;
    auto start = high_resolution_clock::now();
    std::cerr << duration_cast<milliseconds>(high_resolution_clock::now() - start).count() << " ms 1\n" ;
    start = high_resolution_clock::now();

    SDL_DisplayMode display;
    SDL_GetCurrentDisplayMode(0, &display);
    std::cerr << duration_cast<milliseconds>(high_resolution_clock::now() - start).count() << " ms 2\n" ;
    start = high_resolution_clock::now();

    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    width = display.w; height = display.h;
    if(fullscreen) {
        window = SDL_CreateWindow("Hyperbolic",  0, 0, display.w, display.h, SDL_WINDOW_BORDERLESS | SDL_WINDOW_OPENGL);
        SDL_SetWindowFullscreen(window, SDL_WINDOW_FULLSCREEN_DESKTOP);
    } else {
        window = SDL_CreateWindow("Hyperbolic",  display.w/4, display.h/4, display.w/2, display.h/2,  SDL_WINDOW_OPENGL);
    }
    context = SDL_GL_CreateContext(window);
    glGenBuffers(1, &selectionFramebuffer);
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


    // During init, enable debug output
    glEnable              ( GL_DEBUG_OUTPUT );
    glDebugMessageCallback( MessageCallback, 0 );


    glGenFramebuffers(1, &selectionFramebuffer);
    glBindFramebuffer(GL_FRAMEBUFFER, selectionFramebuffer);
    glGenTextures(1, &selectionTexture);
    glBindTexture(GL_TEXTURE_2D, selectionTexture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0,  GL_RGBA,  GL_UNSIGNED_BYTE, NULL);//selectionTexture,
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, selectionTexture, 0);

    // Create the texture object for the depth buffer
    glGenTextures(1, &selectionDepth);
    glBindTexture(GL_TEXTURE_2D, selectionDepth);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, width, height, 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, selectionDepth, 0);


//     Verify that the FBO is correct
    GLenum Status = glCheckFramebufferStatus(GL_FRAMEBUFFER);

    if (Status != GL_FRAMEBUFFER_COMPLETE) {
        printf("FB error, status: 0x%x\n", Status);
        exit(1);
    }

    // Restore the default framebuffer
    glBindTexture(GL_TEXTURE_2D, 0);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

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
    double sinhdr = sinh(r*1.4);
    double msinhdr = sinh(-r*1.4);
    Matrix44 y1 = moveAlongY(-r*1.4);
    Matrix44 y2= moveAlongY(r*1.4);
    Point innerCorner = moveAlongX(-r*1.4)*moveAlongY(-r*1.4)*origin;
    Point yr1 = y1*origin;
    Point yr2 = y2*origin;

    Point rightOutp = moveAlongX(-r)*moveAlongY(-size)*origin;
    Point leftOutp = moveAlongX(r)*moveAlongY(-size)*origin;
    Point outerCorner = moveAlongX(r*1.4)*moveAlongY(r*1.4)*origin;
    Point lastPoint = rightInp;
    Point uic  =movePerpendicularlyToOxy(r, innerCorner);
    Point lic  =movePerpendicularlyToOxy(-r, innerCorner);
//    std::vector<Point> upperFace;
//    std::vector<Point> lowerFace;
    for(const Point& newPoint : {leftInp,
                            outerCorner,
        leftOutp,
                            rightOutp,
                            innerCorner,
                            rightInp})
    {
        Point unp = movePerpendicularlyToOxy(r, newPoint);
        Point lnp = movePerpendicularlyToOxy(-r, newPoint);
//        upperFace.push_back(unp);
//        lowerFace.push_back(lnp);
        Point ulp = movePerpendicularlyToOxy(r, lastPoint);
        Point llp = movePerpendicularlyToOxy(-r, lastPoint);
        res.push_back({{0.0, 0.0, 1.0, 1}, {Polygon, {unp, ulp, llp, lnp}}});
        res.push_back({{0.0, 0.0, 1.0, 1}, {Polygon, {unp, ulp, uic}}});
        res.push_back({{0.0, 0.0, 1.0, 1}, {Polygon, {lnp, llp, lic}}});
        lastPoint = newPoint;
    }
//    res.push_back({{0.0, 0.0, 1.0, 1}, {Polygon, upperFace}});
//    res.push_back({{0.0, 0.0, 1.0, 1}, {Polygon, lowerFace}});
    return res;
}
const Mesh G::cube(double size) {// -- aaa-aba aba-abb abb-aab aab-aaa
    static Mesh res;
    static bool init = false;
    if(init) {
        return res;
    }
    init = true;

    Point pts[2][2][2];
    for(int i = 0; i < 2; i++)
        for(int j = 0; j < 2; j++)
            for(int k = 0; k < 2; k++)
                pts[i][j][k] = moveAlongX((i*2-1)*size)*
                        moveAlongY((j*2-1)*size)*
                        moveAlongZ((k*2-1)*size)*origin;
    for(int i = 0; i < 2; i++) {
        res.push_back({{1.0, 0.0, 0.0, 1}, {Polygon, {pts[0][0][i],
                                                      pts[0][1][i],
                                                      pts[1][1][i],
                                                      pts[1][0][i]}}});
        res.push_back({{1.0, 0.0, 0.0, 1}, {Polygon, {pts[0][i][0],
                                                      pts[0][i][1],
                                                      pts[1][i][1],
                                                      pts[1][i][0]}}});
        res.push_back({{1.0, 0.0, 0.0, 1}, {Polygon, {pts[i][0][0],
                                                      pts[i][0][1],
                                                      pts[i][1][1],
                                                      pts[i][1][0]}}});
    }
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
