TEMPLATE = app
CONFIG += console
#CONFIG +=  c++17

#CONFIG += c++14
QMAKE_CXXFLAGS -= -Wall   -isystem /usr/include
#-std=gnu++1z
QMAKE_DEFAULT_INCDIRS =
QMAKE_CXXFLAGS += -std=gnu++2a -I/usr/include/freetype2 -I/usr/include/libpng16  -Werror -Wno-misleading-indentation -Wno-comment -Wno-sign-compare -Wno-unused-variable -Wno-unused-but-set-variable -O0 # -Wno-missing-braces -Wno-unused-variable -Wno-sign-compare -Wno-missing-field-initializers -Wno-comment#-I/usr/include
LIBS+= -I/usr/include/freetype2 -I/usr/include/libpng16 -lGL -lSDL2 -lglut -lfreetype -lSDL2_ttf
#LIBS+= -lSDL
#QMAKE_CXX = g++-5
QMAKE_CFLAGS += -Wno-unused-variable -Wno-unused-but-set-variable -Wno-sign-compare -Wno-missing-field-initializers -Wno-misleading-indentation  -I/usr/include/freetype2 -I/usr/include/libpng16
#-I/usr/include -stdlib=libstdc++
CONFIG -= app_bundle
CONFIG -= qt
#LIBS += -lstdc++ -L/usr/lib/gcc/i686-linux-gnu/8/
INCLUDEPATH += /usr/include fmt/include
SOURCES += main.cpp \
    commongraphics.cpp \
    util/hyperbolic.cpp \
    util/lodepng.cpp \
    util/example_opengl.cpp \
    game/level.cpp \
    game/gameloop.cpp \
    game/graphics.cpp \
    util/physics.cpp \
    game/console.cpp \
    fmt/src/format.cc \
    fmt/src/os.cc

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
    game/level.h \
    fmt/include/fmt/chrono.h \
    fmt/include/fmt/color.h \
    fmt/include/fmt/compile.h \
    fmt/include/fmt/core.h \
    fmt/include/fmt/format-inl.h \
    fmt/include/fmt/format.h \
    fmt/include/fmt/locale.h \
    fmt/include/fmt/os.h \
    fmt/include/fmt/ostream.h \
    fmt/include/fmt/posix.h \
    fmt/include/fmt/printf.h \
    fmt/include/fmt/ranges.h
