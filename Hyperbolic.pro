TEMPLATE = app
CONFIG += console
CONFIG += c++14
#CONFIG += c++14
QMAKE_CXXFLAGS -= -Wall -std=gnu++11
QMAKE_CXXFLAGS += -std=gnu++14 -Werror -Wno-comment -Wno-sign-compare -Wno-unused-variable -O0 # -Wno-missing-braces -Wno-unused-variable -Wno-sign-compare -Wno-missing-field-initializers -Wno-comment#-I/usr/include
LIBS+= -lGL -lSDL2 -lglut
#QMAKE_CXX = g++-5
QMAKE_CFLAGS += -stdlib=libstdc++ -lglut -lfreeglut -Wno-unused-variable -Wno-sign-compare -Wno-missing-field-initializers
#-I/usr/include
CONFIG -= app_bundle
CONFIG -= qt
#LIBS += -lstdc++ -L/usr/lib/gcc/i686-linux-gnu/8/
INCLUDEPATH += /usr/include
SOURCES += main.cpp \
    commongraphics.cpp \
    util/hyperbolic.cpp

HEADERS += \
    util/hyperbolic.h \
    util/linear.h \
    util/physics.h \
    editor/editor.h \
    util/shittyreflection.h \
    game/gameloop.h \
    game/graphics.h \
    commongraphics.h
