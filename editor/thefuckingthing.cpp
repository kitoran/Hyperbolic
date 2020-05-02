#include "thefuckingthing.h"
//#include <GL/gl.h>
//#include <GL/glu.h>
#include <QDebug>
#include <QPainter>
#include <QPaintEngine>
#include <QOpenGLShaderProgram>
#include <QOpenGLTexture>
#include <QCoreApplication>
#include <math.h>
#include "level.h"

TheFuckingThing::TheFuckingThing(QWidget *mw)
    : mp_program1(0),
      mp_texture(0)
{
    setMinimumSize(300, 250);
}

TheFuckingThing::~TheFuckingThing()
{
    // And now release all OpenGL resources.
    makeCurrent();
    delete mp_texture;
    delete mp_program1;
    delete mp_vshader1;
    delete mp_fshader1;
    m_vbo1.destroy();
    doneCurrent();
}

void TheFuckingThing::drawLevel()
{
    mp_program1->enableAttributeArray(m_vertexAttr1);
//    mp_program1->enableAttributeArray(m_normalAttr1);
    m_vbo1.bind();
//    populateVertexBuffer();

    std::vector<GLfloat> r;

    for(const ColoredEntity &frfr : level.mesh) {
        for(const H::Point& p: frfr.e.p) {
            r.push_back(p.x);
            r.push_back(p.y);
            r.push_back(p.z);
        }
    }

    m_vbo1.allocate(r.data(), sizeof(r[0])*r.size());
    // The data in the buffer is placed like this:
    // vertex1.x, vertex1.y, vertex1.z, normal1.x, normal1.y, normal1.z, vertex2.x, ...
    mp_program1->setAttributeBuffer(m_vertexAttr1, GL_FLOAT, 0, 3, 3 * sizeof(GLfloat));
//    mp_program1->setAttributeBuffer(m_vertexAttr1, GL_FLOAT, 0, 3, 0);
//    mp_program1->setAttributeBuffer(m_normalAttr1, GL_FLOAT, 3 * sizeof(GLfloat), 3, 6 * sizeof(GLfloat));
    m_vbo1.release();

    glDrawArrays(GL_TRIANGLES, 0, r.size()/3);//numberOfVertices);

//    mp_program1->disableAttributeArray(m_normalAttr1);
    mp_program1->disableAttributeArray(m_vertexAttr1);
}

void TheFuckingThing::initializeGL()
{
    initializeOpenGLFunctions();

//    mp_texture = new QOpenGLTexture(QImage(":/qt.png"));

    mp_vshader1 = new QOpenGLShader(QOpenGLShader::Vertex);
    const char *vsrc1 =
        "attribute highp vec4 vertex;\n"
        "uniform mediump mat4 matrix;\n"
        "varying mediump vec4 color;\n"
        "void main(void)\n"
        "{\n"
        "    color = vec4(1.0, 0.0, 1.0, 1.0);\n"
        "    color = clamp(color, 0.0, 1.0);\n"
        "    gl_Position = matrix * vertex;\n"
        "}\n";
    qDebug() << "compile:" << mp_vshader1->compileSourceCode(vsrc1);

    mp_fshader1 = new QOpenGLShader(QOpenGLShader::Fragment);
    const char *fsrc1 =
        "varying mediump vec4 color;\n"
        "void main(void)\n"
        "{\n"
        "    gl_FragColor = color;\n"
        "}\n";
    mp_fshader1->compileSourceCode(fsrc1);

    mp_program1 = new QOpenGLShaderProgram;
    mp_program1->addShader(mp_vshader1);
    mp_program1->addShader(mp_fshader1);
    mp_program1->link();

    m_vertexAttr1 = mp_program1->attributeLocation("vertex");
//    m_normalAttr1 = mp_program1->attributeLocation("normal");
    m_matrixUniform1 = mp_program1->uniformLocation("matrix");


    // Use a vertex buffer object. Client-side pointers are old-school and should be avoided.
    m_vbo1.create();


}

void TheFuckingThing::paintGL()
{

    QPainter painter;
    painter.begin(this);

    painter.beginNativePainting();

    QColor b = Qt::black;
    glClearColor(b.redF(), b.greenF(), b.blue(), b.alphaF());
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glFrontFace(GL_CW);
    glCullFace(GL_FRONT);
    glEnable(GL_CULL_FACE);
    glEnable(GL_DEPTH_TEST);

    mp_program1->bind();
    mp_program1->setUniformValue(m_matrixUniform1, m_modelView);
    drawLevel();
    mp_program1->release();

    glDisable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);

    painter.endNativePainting();



    if (const int elapsed = m_time.elapsed()) {
        QString framesPerSecond;
        framesPerSecond.setNum(m_frames /(elapsed / 1000.0), 'f', 2);
        painter.setPen(Qt::white);
        painter.drawText(20, 40, framesPerSecond + " paintGL calls / s");
    }

    painter.end();

    if (!(m_frames % 100)) {
        m_time.start();
        m_frames = 0;
    }
    ++m_frames;

    // When requested, follow the ideal way to animate: Rely on
    // blocking swap and just schedule updates continuously.
        update();
}


void TheFuckingThing::populateVertexBuffer()
{
        // For the cube all the data belonging to the texture coordinates and
        // normals is placed separately, after the vertices. Here, for the Qt logo,
        // let's do something different and potentially more efficient: create a
        // properly interleaved data set.

        std::vector<GLfloat> fewfwe;
        for(const ColoredEntity& dwdw : level.mesh) {
            m_vbo1.bind();
            m_vbo1.allocate(dwdw.e.p.data(),
                            dwdw.e.p.size()*sizeof(dwdw.e.p[0]));
//            fewfwe.insert(end.fewfwe dwdw.p.begin(),
            m_vbo1.release();
            glDrawArrays(GL_TRIANGLE_FAN, 0, dwdw.e.p.size());
        }

}

void TheFuckingThing::resizeGL(int, int)
{
//    if (m_hasButton) {
//        if (!m_btn) {
//            m_btn = new QPushButton("A widget on top.\nPress for more widgets.", this);
//            connect(m_btn, &QPushButton::clicked, this, &TheFuckingThing::handleButtonPress);
//        }
//        m_btn->move(20, 80);
//    }
}

void TheFuckingThing::handleButtonPress()
{
//    m_mainWindow->addNew();
}
