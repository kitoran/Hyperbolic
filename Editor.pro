TEMPLATE = app
CONFIG += gui
QT += widgets core
QMAKE_DEFAULT_INCDIRS =
QMAKE_CXXFLAGS += -std=gnu++1z  -I/usr/include/freetype2 -I/usr/include/libpng16  -Werror -Wno-misleading-indentation -Wno-comment -Wno-sign-compare -Wno-unused-variable -Wno-unused-but-set-variable -O0 # -Wno-missing-braces -Wno-unused-variable -Wno-sign-compare -Wno-missing-field-initializers -Wno-comment#-I/usr/include
LIBS+= -I/usr/include/freetype2 -I/usr/include/libpng16 -lGL -lSDL2 -lglut -lfreetype -lSDL2_ttf
LIBS+= -L/usr/lib/x86_64-linux-gnu/ -lboost_iostreams -lboost_serialization
#QMAKE_CXX = g++-5
QMAKE_CFLAGS +=  -lglut -lfreeglut -Wno-unused-variable -Wno-unused-but-set-variable -Wno-sign-compare -Wno-missing-field-initializers -Wno-misleading-indentation  -I/usr/include/freetype2 -I/usr/include/libpng16
#-I/usr/include -stdlib=libstdc++
CONFIG -= app_bundle
#LIBS += -lstdc++ -L/usr/lib/gcc/i686-linux-gnu/8/
INCLUDEPATH += /usr/include
SOURCES += commongraphics.cpp \
    util/hyperbolic.cpp \
    editor/editor.cpp \
    util/lodepng.cpp \
    util/example_opengl.cpp \
    util/physics.cpp \
    editor/main.cpp \
    editor/editorwindow.cpp \
    editor/level.cpp

HEADERS += \
    util/hyperbolic.h \
    util/linear.h \
    util/physics.h \
    editor/editor.h \
    util/shittyreflection.h \
    commongraphics.h \
    editor/statedeclarations.h \
    util/lodepng.h \
    editor/editorwindow.h \
    editor/level.h

FORMS += \
    editor/editorwindow.ui
