//#define _GLIBCXX_INCLUDE_NEXT_C_HEADERSZ
#include "commongraphics.h"
#include "game/gameloop.h"
#include "editor/editor.h"
#include "game/console.h"
#include <iostream>
inline int global = 6;
int main(int argc, char ** argv) {
//    if (argc < 2) {
//      // report version
//      std::cout << argv[0] << " Version " << 0 << "."
//                << 1 << std::endl;
//      std::cout << "Usage: " << argv[0] << " number" << std::endl;
//      return 1;
//    }
//    std::cout.setf(std::ios::unitbuf);
    auto rewfw = normalizeWass3 (origin3);
    auto fwef  = normalizeWass3 ({sinh(1), 0, cosh(1)});
    auto sergs = form3(rewfw, fwef);
//     - (form3 (normalizeWass3 (a), normalizeWass3 (b)));
    auto  efwefw = chDistance3 (origin3, {sinh(1), 0, cosh(1)});

    auto eaergaerg =  distance3( origin3, {sinh(1), 0, cosh(1)});
    auto fwehtrf = moveRightTo3({sinh(1), 0, cosh(1)});
////    using namespace H;
    printf("%s", get_current_dir_name());
//    return 0;
//    double a1 = rand();
//    double b1 = rand();
//    double c1 = rand();
//    double a2 = rand();
//    double b2 = rand();
//    double c2 = rand();

//    Vector2 coords = inv22({{a1, b1, a2, b2}})*Vector2{c1, c2};
//    auto deb1 = coords.x * a1 + coords.y*b1 - c1;
//    assert(fabs(deb1) < 0.01);

            G::initialiseGraphics(argc, argv);
            loadHistory();
            auto d = SDL_GetWindowPixelFormat( window );
            printf("%d\n", d);

            printf("%s\n", "beforeEverything");
            printf("%ld %lld\n", sizeof(size_t), 1366*768*4 * 60 * 15ll);

//            G::framee = (u_char*)malloc(1366*768*4 * 60 * 15ll);
            perror("malloc:");
            fprintf(stderr,
                    "%p\n", G::framee);
            gameLoop( );
            return 0;
//    editorLoop();

            //            G::deinitializeGraphics();
}
