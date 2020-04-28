#include "commongraphics.h"
#include "editor/editor.h"
#include "editor/editorwindow.h"
#include <iostream>
#include <QApplication>
int main(int argc, char ** argv) {
    printf("%s", get_current_dir_name());
    QApplication app(argc,argv);

//    G::initialiseGraphics(argc, argv);
    EditorWindow e;
    e.show();
    return app.exec();
}
