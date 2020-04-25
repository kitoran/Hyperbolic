TEMPLATE = app
CONFIG += console
#CONFIG +=  c++17

#CONFIG += c++14
QMAKE_CXXFLAGS -= -Wall -std=gnu++14  -isystem /usr/include
#-std=gnu++1z
QMAKE_DEFAULT_INCDIRS =
QMAKE_CXXFLAGS += -std=gnu++1z  -I/usr/include/freetype2 -I/usr/include/libpng16  -Werror -Wno-misleading-indentation -Wno-comment -Wno-sign-compare -Wno-unused-variable -Wno-unused-but-set-variable -O0 # -Wno-missing-braces -Wno-unused-variable -Wno-sign-compare -Wno-missing-field-initializers -Wno-comment#-I/usr/include
LIBS+= -I/usr/include/freetype2 -I/usr/include/libpng16 -lGL -lSDL2 -lglut -lfreetype -lSDL2_ttf
#LIBS+= -lSDL
#QMAKE_CXX = g++-5
QMAKE_CFLAGS +=  -lglut -lfreeglut -Wno-unused-variable -Wno-unused-but-set-variable -Wno-sign-compare -Wno-missing-field-initializers -Wno-misleading-indentation  -I/usr/include/freetype2 -I/usr/include/libpng16
#-I/usr/include -stdlib=libstdc++
CONFIG -= app_bundle
CONFIG -= qt
#LIBS += -lstdc++ -L/usr/lib/gcc/i686-linux-gnu/8/
INCLUDEPATH += /usr/include
SOURCES += main.cpp \
    commongraphics.cpp \
    util/hyperbolic.cpp \
    util/lodepng.cpp \
    util/example_opengl.cpp \
    game/level.cpp \
    game/gameloop.cpp \
    game/graphics.cpp \
    util/physics.cpp \
    game/console.cpp

HEADERS += \
    util/hyperbolic.h \
    util/linear.h \
    util/physics.h \
    util/shittyreflection.h \
    game/gameloop.h \
    game/graphics.h \
    commongraphics.h \
    game/console.h \
    util/lodepng.h \
    game/level.h
