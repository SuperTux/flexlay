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
#include <QMouseEvent>

#include <iostream>

#include "input_event.hpp"
#include "workspace.hpp"
#include "graphic_context.hpp"
#include "gui/editor_map_component.hpp"

class EditorMapComponent;
class EditorMapComponentImpl;
class EditorMapWidget;
class QWidget;
class Scrollbar;

class EditorMapWidget : public QGLWidget
{
  Q_OBJECT
private:
  EditorMapComponent& m_comp;

public:
  EditorMapWidget(EditorMapComponent& comp, QWidget* parent) :
    QGLWidget(QGLFormat(QGL::SampleBuffers), parent),
    m_comp(comp)
  {
    setAutoFillBackground(true);
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

  void mouseMoveEvent(QMouseEvent* event) override
  {
    Workspace workspace = m_comp.get_workspace();
    InputEvent ev(*event);
    workspace.mouse_move(ev);
    std::cout << "mouse move: " << std::endl;
    repaint();
  }

  void mousePressEvent(QMouseEvent* event) override
  {
    Workspace workspace = m_comp.get_workspace();
    InputEvent ev(*event);
    workspace.mouse_down(ev);
    std::cout << "mouse press: " << std::endl;
    repaint();
  }

  void mouseReleaseEvent(QMouseEvent* event) override
  {
    Workspace workspace = m_comp.get_workspace();
    InputEvent ev(*event);
    workspace.mouse_up(ev);
    std::cout << "mouse release: " << std::endl;
    repaint();
  }

  void paintGL() override
  {
    std::cout << "Paint" << std::endl;

    QPainter painter;
    painter.begin(this);

    //painter.setRenderHint(QPainter::Antialiasing);

    Workspace workspace = m_comp.get_workspace();
    GraphicContext gc(m_comp.get_gc_state(), painter);
    m_comp.get_gc_state().push(gc);
    workspace.draw(gc);
    m_comp.get_gc_state().pop(gc);

    painter.end();
  }

  void resizeGL(int width, int height) override
  {
    m_comp.get_gc_state().set_size(width, height);
  }
};

#endif

/* EOF */
