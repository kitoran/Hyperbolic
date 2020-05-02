#ifndef TRIANGLE_H
#define TRIANGLE_H

#include <QMainWindow>

namespace Ui {
class Triangle;
}

class Triangle : public QMainWindow
{
    Q_OBJECT

public:
    explicit Triangle(QWidget *parent = 0);
    ~Triangle();


private:
    Ui::Triangle *ui;
    void update();
};

#endif // TRIANGLE_H
