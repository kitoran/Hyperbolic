#include "commongraphics.h"
#include "editor/editor.h"
#include "editor/editorwindow.h"
#include <iostream>
#include <QApplication>

QtMessageHandler stdHandler;
void myHandler(QtMsgType type, const QMessageLogContext &context, const QString &message) {
//  Этот хендлер нужен чтобы ставить точки останова на вывод qt о проблемах
    if(message.contains("QAbstractSocket::connectToHost() called when already looking up")) {
        qt_noop();
    }
    stdHandler(type, context, message);
}

int main(int argc, char ** argv) {
    printf("%s", get_current_dir_name());

    stdHandler =
            qInstallMessageHandler(myHandler);
    QApplication app(argc,argv);

//    G::initialiseGraphics(argc, argv);
    EditorWindow e;
    e.show();
    return app.exec();
}
