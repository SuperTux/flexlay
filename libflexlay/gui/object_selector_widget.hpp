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

#ifndef HEADER_OBJECT_SELECTOR_WIDGET_HPP
#define HEADER_OBJECT_SELECTOR_WIDGET_HPP

#include <QWidget>
#include <boost/signals2.hpp>

#include "math/point.hpp"
#include "object_brush.hpp"

class ObjectSelectorWidget : public QWidget
{
private:
  int width;
  int height;

  int obj_width;
  int obj_height;

  int index;

  int offset;
  int old_offset;
  int mouse_over_tile;
  bool scrolling;
  Point click_pos;
  Point mouse_pos;
  float scale;

  std::vector<ObjectBrush> brushes;
  int drag_obj;

  boost::signals2::signal<void (ObjectBrush, Point)> on_drop;

public:
  ObjectSelectorWidget(int obj_w, int obj_h, QWidget* parent);
  ~ObjectSelectorWidget();

  void add_brush(const ObjectBrush& brush);

  boost::signals2::signal<void (ObjectBrush, Point)>& sig_drop();

protected:
  void mousePressEvent(QMouseEvent* event) override;
  void mouseReleaseEvent(QMouseEvent* event) override;
  void mouseMoveEvent(QMouseEvent* event) override;
  void wheelEvent(QWheelEvent* event) override;

  void paintEvent(QPaintEvent* event) override;
  void resizeEvent(QResizeEvent* event) override;

  QSize minimumSizeHint() const override;
  QSize sizeHint() const override;

private:
  ObjectSelectorWidget(const ObjectSelectorWidget&) = delete;
  ObjectSelectorWidget& operator=(const ObjectSelectorWidget&) = delete;
};

#endif

/* EOF */
