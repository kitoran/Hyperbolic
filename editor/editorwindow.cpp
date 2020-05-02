#include "editorwindow.h"
#include "level.h"
#include <fstream>
#include "ui_editorwindow.h"
Environment levelPentagon() {
    auto X = moveAlongX (acosh((sqrt(5)+1)/2));
    auto Y = moveAlongY (acosh((sqrt(5)+1)/2));
    auto Z = moveAlongZ (acosh((sqrt(5)+1)/2));
    Point p0 = H::origin;
    Point p1 = X * p0;
    Point p2 = X * Y * p0;
    Point p3 = Y * X * p0;
    Point p4 = Y * p0;
    Environment res;
    res.mesh = Mesh {/*((1.0, 0.0, 0.0, 1.0),
        //                                              (P.Polygon ) $
        //                                              map ((\a -> rotateAroundZ a !$ (Point 1 0 0 1)) .
        //                                                   (/360.0) .
        //                                                   (*(tau::Double)) .
        //                                                   fromIntegral::Integer->Point Double) $ [0..359]),*/
                          {{1, 0, 0, 1}, {Polygon, {p0, p1, p2, p3, p4}}}};
    Obstacle o;
    o.type = Triangle;
    o.a = p0;
    o.b = p1;
    o.c = p2;
    Obstacle o1;
    o1.type = Triangle;
    o1.a = p0;
    o1.b = p2;
    o1.c = p3;
    Obstacle o2;
    o2.type = Triangle;
    o2.a = p0;
    o2.b = p3;
    o2.c = p4;
    o.thickness = o1.thickness = o2.thickness = 0.01;
    res.obstacles = Obstacles{o, o1, o2};


    Environment dod;
    Environment floor = res;
    floor.mesh.front().color = Color{1,1,1,1};
    Environment xrotater = rotateAroundX(M_PI_2) * res;
    xrotater.mesh.front().color = Color{0,0,1,1};

    dod += floor;
    dod += xrotater;
    dod += rotateAroundY(-M_PI_2) * res;

    dod += Y * rotateAroundX(M_PI_2) * res;
    dod += X * rotateAroundY(-M_PI_2) * res;
    dod += Z * res;
    dod += moveFromTo(p1, p2, acosh((sqrt(5)+1)/2)) * rotateAroundX(M_PI_2) * res;
    dod += moveFromTo(p4, p3, acosh((sqrt(5)+1)/2)) * rotateAroundY(-M_PI_2) * res;
    dod += X * Z * rotateAroundY(M_PI) * res;
    dod += Y * Z * rotateAroundX(M_PI) *res;
    Environment one =  X * Y * Z * rotateAroundX(M_PI) *res;
    Environment other = X * Y * Z * res;
    one.mesh.front().color = Color{0,1,0,1};
    other.mesh.front().color = Color{1,1,0,1};
    dod += one;
    dod += other;
    return moveAlongX(-0.2)*moveAlongY(-0.2)*dod;
//        red = (1.0, 0.0, 0.0, 1)
}
EditorWindow::EditorWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::EditorWindow)
{
    ui->setupUi(this);
//    level.mesh = levelPentagon().mesh;

    std::vector<GLfloat> r = {0, 1, 0, //0,0,1,
                   -1, 0, 0, //0,0,1,
                              0.75, -0.75, 0,//0,0,1
                              0.75, -0.75, 0,//0,0,1
                              0.75, -1, 0,//0,0,1
                              1, 0.75, 0,//0,0,1

                  };
    for(int i = 0; i < r.size(); i+=9) {
        level.mesh.push_back(
        {{1,0,0,1},{Polygon, {{r[i], r[i+1], r[i+2], 0},
                            {r[i+3], r[i+4], r[i+5], 0},
                            {r[i+6], r[i+7], r[i+8], 0}}}}
                    );
    }

}

EditorWindow::~EditorWindow()
{
    delete ui;
}

void EditorWindow::on_actionsave_triggered()
{
    FILE* f = fopen(filename.c_str(), "w");
    serialize(f, level);
    fclose(f);
}

void EditorWindow::on_actionload_triggered()
{
    qDebug() << "fwfwe" << level.mesh.size();
    qDebug() << level.mesh[1].e.p.size();
    qDebug() << level.mesh[1].e.p[2].y;
    FILE* f = fopen(filename.c_str(), "r");
    if(!f) {
        qDebug() << "couldnt" << filename.c_str();
    }
    deserialize(f, &level);
    fclose(f);

    qDebug() << "af" << level.mesh.size();
    qDebug() << level.mesh[1].e.p[2].y;
}
