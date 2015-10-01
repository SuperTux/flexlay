// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
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

#ifndef HEADER_MINIMAP_WIDGET_HPP
#define HEADER_MINIMAP_WIDGET_HPP

#include <QWidget>

#include "editor_map.hpp"
#include "sprite.hpp"

class EditorMapComponent;

class MinimapWidget : public QWidget
{
private:
  bool m_drag_active;

  int m_last_serial;
  EditorMap m_editor_map;

  EditorMapComponent* m_parent;
  Sprite m_minimap_surface;

public:
  MinimapWidget(EditorMapComponent* p);

  void update_minimap();

protected:
  void mouseMoveEvent(QMouseEvent* event) override;
  void mousePressEvent(QMouseEvent* event) override;
  void mouseReleaseEvent(QMouseEvent* event) override;

  void paintEvent(QPaintEvent* event) override;

  QSize minimumSizeHint() const override;
  QSize sizeHint() const override;

private:
  MinimapWidget(const MinimapWidget&) = delete;
  MinimapWidget& operator=(const MinimapWidget&) = delete;
};

#endif

/* EOF */
