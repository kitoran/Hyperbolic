TEMPLATE = app
CONFIG += console
QMAKE_CXXFLAGS += -std=gnu++17 -fno-rtti -lglut -Wno-unused-variable -Wno-sign-compare
LIBS+=-lglut -lGL -lSDL2
QMAKE_CFLAGS += -std=gnu++17 -fno-rtti -lglut -lfreeglut -Wno-unused-variable -Wno-sign-compare
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
