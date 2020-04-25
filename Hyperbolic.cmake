
set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} -std=gnu++1z -I/usr/include/freetype2 -I/usr/include/libpng16 -std=gnu++14 -Werror -Wno-misleading-indentation -Wno-comment -Wno-sign-compare -Wno-unused-variable -O0 -lglut -lfreeglut -Wno-unused-variable -Wno-sign-compare -Wno-missing-field-initializers -Wno-misleading-indentation  -I/usr/include/freetype2 -I/usr/include/libpng16")
# -Wno-missing-braces -Wno-unused-variable -Wno-sign-compare -Wno-missing-field-initializers -Wno-comment#-I/usr/include
include_directories(/usr/include/freetype2 /usr/include/libpng16 /usr/include)
link_libraries(GL SDL2 glut freetype SDL2_ttf)
#LIBS+= -lSDL
#QMAKE_CXX = g++-5

#-I/usr/include -stdlib=libstdc++
#CONFIG -= app_bundle
#CONFIG -= qt
#LIBS += -lstdc++ -L/usr/lib/gcc/i686-linux-gnu/8/
#INCLUDEPATH += 
set(SOURCES
    main.cpp 
    commongraphics.cpp 
    util/hyperbolic.cpp 
    editor/editor.cpp 
    util/lodepng.cpp 
    util/example_opengl.cpp 
    game/level.cpp 
    game/gameloop.cpp 
    game/graphics.cpp 
    util/physics.cpp 
    game/console.cpp
)
#
#HEADERS += \
#    util/hyperbolic.h \
#    util/linear.h \
#    util/physics.h \
#    editor/editor.h \
#    util/shittyreflection.h \
#    game/gameloop.h \
#    game/graphics.h \
#    commongraphics.h \
#    game/console.h \
#    editor/statedeclarations.h \
#    util/lodepng.h \
#    game/level.h
#
