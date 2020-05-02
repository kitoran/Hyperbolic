#include "triangle.h"
#include "ui_triangle.h"
#include <math.h>
Triangle::Triangle(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::Triangle)
{
    ui->setupUi(this);

    for(QLineEdit* wef: findChildren<QLineEdit*>()) {
        connect(wef, &QLineEdit::textChanged, this, &Triangle::update);
    }
}

Triangle::~Triangle()
{
    delete ui;
}

double getNumber(QLineEdit* wef) {
    QString line = wef->text();
    if(line.startsWith("pi/")) {
        return M_PI/wef->text().right(line.size()-3).toDouble();
    }
    if(line.startsWith("tau/")) {
        return M_PI*2/wef->text().right(line.size()-4).toDouble();
    }
    return line.toDouble();
}

void Triangle::update()
{
    double A = getNumber(ui->lineEditA);
    double B = getNumber(ui->lineEditB);
    double C = getNumber(ui->lineEditC);

    double c = acosh((cos(C) + cos(A)*cos(B))/(sin(A)*sin(B)));
    double a = acosh((cos(A) + cos(B)*cos(C))/(sin(B)*sin(C)));
    double b = acosh((cos(B) + cos(C)*cos(A))/(sin(C)*sin(A)));
    ui->lineEdita->setText(QString::number(a));
    ui->lineEditb->setText(QString::number(b));
    ui->lineEditc->setText(QString::number(c));
}
