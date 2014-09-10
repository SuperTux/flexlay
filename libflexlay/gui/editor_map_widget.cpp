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

#include "gui/editor_map_widget.hpp"

#include <QDropEvent>
#include <QMouseEvent>
#include <QPainter>
#include <QWidget>
#include <QMimeData>

#include "editor_map.hpp"

EditorMapWidget::EditorMapWidget(EditorMapComponent& comp, QWidget* parent) :
  QWidget(parent),
  m_comp(comp)
{
  QPalette pal = palette();
  pal.setColor(backgroundRole(), QColor(100, 80, 100));
  setPalette(pal);

  setAutoFillBackground(true);
  setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
  setAcceptDrops(true);
}

EditorMapWidget::~EditorMapWidget()
{
}

void
EditorMapWidget::dragEnterEvent(QDragEnterEvent* event)
{
  std::cout << "dragEnter: " << event->mimeData()->hasFormat("application/supertux-badguy") << std::endl;
  if (event->mimeData()->hasFormat("application/supertux-badguy"))
  {
    event->accept();
  }
}

void
EditorMapWidget::dragLeaveEvent(QDragLeaveEvent* event)
{
  std::cout << "dragLeave" << std::endl;
}

void
EditorMapWidget::dropEvent(QDropEvent* event)
{
  std::cout << "drop happened: " << event->pos().x() << " " << event->pos().y() << std::endl;
  QByteArray data = event->mimeData()->data("application/supertux-badguy");
  int i = 0;
  memcpy(&i, data.data(), sizeof(i));
}

QSize
EditorMapWidget::sizeHint() const
{
  return QSize(1280, 800);
}

void
EditorMapWidget::on_map_change()
{
  std::cout << "EditorMapWidget::on_map_change()" << std::endl;
  Workspace workspace = m_comp.get_workspace();
  if (!workspace.is_null())
  {
    if (workspace.get_map() && workspace.get_map().has_bounding_rect())
    {
      Rect rect = workspace.get_map().get_bounding_rect();
      std::cout << "Setting minimum Size: " << rect.get_width() << " " << rect.get_height() << std::endl;
      setMinimumSize(QSize(rect.get_width(), rect.get_height()));

      QPalette pal = palette();
      pal.setColor(backgroundRole(), workspace.get_map().get_background_color().to_qt());
      setPalette(pal);

      repaint();
    }
  }
}

void
EditorMapWidget::mouseMoveEvent(QMouseEvent* event)
{
  Workspace workspace = m_comp.get_workspace();
  InputEvent ev(*event);
  workspace.mouse_move(ev);
  repaint();
}

void
EditorMapWidget::mousePressEvent(QMouseEvent* event)
{
  Workspace workspace = m_comp.get_workspace();
  InputEvent ev(*event);
  workspace.mouse_down(ev);
  std::cout << "mouse press: " << std::endl;
  repaint();
}

void
EditorMapWidget::mouseReleaseEvent(QMouseEvent* event)
{
  Workspace workspace = m_comp.get_workspace();
  InputEvent ev(*event);
  workspace.mouse_up(ev);
  std::cout << "mouse release: " << std::endl;
  repaint();
}

void
EditorMapWidget::paintEvent(QPaintEvent* event)
{
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

void
EditorMapWidget::resizeEvent(QResizeEvent* event)
{
  m_comp.get_gc_state().set_size(event->size().width(), event->size().height());
}

/* EOF */
