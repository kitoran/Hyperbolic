#ifndef EDITORWINDOW_H
#define EDITORWINDOW_H

#include <QMainWindow>

namespace Ui {
class EditorWindow;
}

class EditorWindow : public QMainWindow
{
    Q_OBJECT
    const std::string filename = "ddwa.txt";
public:
    explicit EditorWindow(QWidget *parent = 0);
    ~EditorWindow();

private slots:
    void on_actionsave_triggered();

    void on_actionload_triggered();

private:
    Ui::EditorWindow *ui;
};

#endif // EDITORWINDOW_H
