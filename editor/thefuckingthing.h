#ifndef THEFUCKINGTHING_H
#define THEFUCKINGTHING_H
#include <QOpenGLWidget>
#include <QOpenGLFunctions>

#include <QOpenGLBuffer>
#include <QVector3D>
#include <QMatrix4x4>
#include <QTime>
#include <QVector>
#include <QPushButton>

QT_FORWARD_DECLARE_CLASS(QOpenGLTexture)
QT_FORWARD_DECLARE_CLASS(QOpenGLShader)
QT_FORWARD_DECLARE_CLASS(QOpenGLShaderProgram)

class TheFuckingThing : public QOpenGLWidget, protected QOpenGLFunctions
{
    Q_OBJECT
public:
//    using QOpenGLWidget::QOpenGLWidget;
    TheFuckingThing(QWidget *mw);
    ~TheFuckingThing();


private slots:
    void handleButtonPress();

protected:
    void resizeGL(int w, int h) override;
    void paintGL() override;
    void initializeGL() override;

private:
    void paintTexturedCube();
    void paintQtLogo();
    void createGeometry();


    void populateVertexBuffer();


    QMatrix4x4 m_modelView;
    QTime m_time;
    QOpenGLShader *mp_vshader1;
    QOpenGLShader *mp_fshader1;
    QOpenGLShaderProgram *mp_program1;
    QOpenGLTexture *mp_texture;
    QOpenGLBuffer m_vbo1;
    int m_vertexAttr1;
//    int m_normalAttr1;
    int m_matrixUniform1;
    int m_frames;

    int numberOfVertices = 0;
    void drawLevel();
};


#endif // THEFUCKINGTHING_H
