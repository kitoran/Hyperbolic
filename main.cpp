#include "util/commongraphics.h"
#include "game/gameloop.h"
#include "editor/editor.h"


int main(int argc, char ** argv) {

    using namespace H;
    double a1 = rand();
    double b1 = rand();
    double c1 = rand();
    double a2 = rand();
    double b2 = rand();
    double c2 = rand();

    Vector2 coords = inv22({{a1, b1, a2, b2}})*Vector2{c1, c2};
    auto deb1 = coords.x * a1 + coords.y*b1 - c1;
    assert(fabs(deb1) < 0.01);

            G::initialiseGraphics(argc, argv);
            auto d = SDL_GetWindowPixelFormat( window );
            printf("%d\n", d);

            printf("%s\n", "beforeEverything");
            gameLoop( );
//            G::deinitializeGraphics();
            return 0;
}
