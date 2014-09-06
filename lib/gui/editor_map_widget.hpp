// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HEADER_EDITOR_MAP_WIDGET_HPP
#define HEADER_EDITOR_MAP_WIDGET_HPP

#include <QGLWidget>
#include <QPainter>

#include "graphic_context.hpp"

class EditorMapComponent;
class EditorMapComponentImpl;
class EditorMapWidget;
class QWidget;
class Scrollbar;

class EditorMapWidget : public QGLWidget
{
  //  Q_OBJECT
private:
  EditorMapComponent& m_comp;

public:
  EditorMapWidget(EditorMapComponent& comp, QWidget* parent) :
    QGLWidget(QGLFormat(QGL::SampleBuffers), parent),
    m_comp(comp)
  {
    setAutoFillBackground(false);
    setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
  }

  virtual ~EditorMapWidget()
  {}

protected:
  QSize minimumSizeHint() const override
  {
    return QSize(640, 480);
  }
  
  QSize sizeHint() const override
  {
    return QSize(1280, 800);
  }

  void paintGL() override
  {
    std::cout << "Paint" << std::endl;

    glClearColor(1.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);

    QPainter painter;
    painter.begin(this);
    painter.setRenderHint(QPainter::Antialiasing);

    Workspace workspace = m_comp.get_workspace();
    GraphicContextState state(width(), height());
    GraphicContext gc(state, painter);
    workspace.draw(gc);

    painter.rotate(10.0f);
    painter.translate(100, 100);

    painter.fillRect(QRect(50, 50, 50, 50), QColor(255, 255, 255));

    painter.end();
  }

  void resizeGL(int width, int height) override
  {
    std::cout << "resizing: " << width << "x" << height << std::endl;
    
    int side = qMin(width, height);
    glViewport((width - side) / 2, (height - side) / 2, side, side);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(-0.5, +0.5, -0.5, +0.5, 4.0, 15.0);
    glMatrixMode(GL_MODELVIEW);
  }
};

#endif

/* EOF */
