TEMPLATE = app
CONFIG += console
QMAKE_CXXFLAGS += -std=gnu++1y -fno-rtti -lglut -Wno-unused-variable -Wno-sign-compare -Wno-missing-field-initializers #-I/usr/include
LIBS+=-lglut -lGL -lSDL2
QMAKE_CXX = g++-5
QMAKE_CFLAGS += -stdlib=libstdc++ -std=gnu++14 -fno-rtti -lglut -lfreeglut -Wno-unused-variable -Wno-sign-compare -Wno-missing-field-initializers
#-I/usr/include
CONFIG -= app_bundle
CONFIG -= qt
#LIBS += -lstdc++ -L/usr/lib/gcc/i686-linux-gnu/8/
INCLUDEPATH += /usr/include
SOURCES += main.cpp \
    util/hyperbolic.cpp \
    util/commongraphics.cpp

HEADERS += \
    util/hyperbolic.h \
    util/linear.h \
    util/physics.h \
    util/commongraphics.h \
    editor/editor.h \
    util/shittyreflection.h \
    game/gameloop.h \
    game/graphics.h
