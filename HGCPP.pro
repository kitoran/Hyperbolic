TEMPLATE = app
CONFIG += console
QMAKE_CXXFLAGS += -std=gnu++17 -fno-rtti -lglut
LIBS+=-lglut -lGL -lSDL2
QMAKE_CFLAGS += -std=gnu++17 -fno-rtti -lglut -lfreeglut
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += main.cpp \
    util/hyperbolic.cpp \
    util/commongraphics.cpp

HEADERS += \
    util/hyperbolic.h \
    util/linear.h \
    util/physics.h \
    util/commongraphics.h \
    editor/editor.h \
    util/shittyreflection.h
