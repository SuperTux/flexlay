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

#include <iostream>

#include <QWidget>

#include "input_event.hpp"
#include "workspace.hpp"
#include "graphic_context.hpp"
#include "gui/editor_map_component.hpp"

class EditorMapComponent;
class EditorMapComponentImpl;
class EditorMapWidget;
class QWidget;
class Scrollbar;

class EditorMapWidget : public QWidget
{
  Q_OBJECT
private:
  EditorMapComponent& m_comp;

public:
  EditorMapWidget(EditorMapComponent& comp, QWidget* parent = nullptr);
  virtual ~EditorMapWidget();

  void on_map_change();

protected:
  void dragEnterEvent(QDragEnterEvent* event) override;
  void dragLeaveEvent(QDragLeaveEvent* event) override;
  void dropEvent(QDropEvent* event) override;

  void mouseMoveEvent(QMouseEvent* event) override;
  void mousePressEvent(QMouseEvent* event) override;
  void mouseReleaseEvent(QMouseEvent* event) override;

  void paintEvent(QPaintEvent* event) override;

  QSize sizeHint() const;
  void resizeEvent(QResizeEvent* event) override;
};

#endif

/* EOF */
