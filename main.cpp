#include "util/commongraphics.h"
#include "editor/editor.h"


int main(int argc, char ** argv) {

            G::initialiseGraphics(argc, argv);
            auto d = SDL_GetWindowPixelFormat( window );
            printf("%d\n", d);

            printf("%s\n", "beforeEverything");
            editorLoop( );
//            G::deinitializeGraphics();
            return 0;
}
