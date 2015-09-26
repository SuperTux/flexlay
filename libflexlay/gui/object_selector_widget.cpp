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

#include "gui/object_selector_widget.hpp"

#include <QByteArray>
#include <QDrag>
#include <QMimeData>
#include <QPainter>
#include <QResizeEvent>
#include <QScrollArea>

#include "graphic_context.hpp"

struct SuperTuxBadGuyData
{
  int type;
};

ObjectSelectorWidget::ObjectSelectorWidget(int cell_w, int cell_h, QWidget* viewport, QWidget* parent) :
  QWidget(parent),
  m_viewport(viewport),
  m_cell_width(cell_w), 
  m_cell_height(cell_h),
  m_brushes(),
  m_has_focus(false)
{
  index = 0;

  mouse_over_tile = -1;
  scrolling = false;
  scale = 1.0f;
  drag_obj = -1;

  setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Expanding);
  setMouseTracking(true);
}

ObjectSelectorWidget::~ObjectSelectorWidget()
{
}

QSize
ObjectSelectorWidget::minimumSizeHint() const
{
  int columns = get_columns();
  int min_rows = (m_brushes.size() + columns - 1) / columns;
  return QSize(m_cell_width * get_columns(), 
               m_cell_height * min_rows);
}

void
ObjectSelectorWidget::resizeEvent(QResizeEvent* event)
{
}

int
ObjectSelectorWidget::get_columns() const
{
  return m_viewport->width() / m_cell_width;
}

void
ObjectSelectorWidget::mousePressEvent(QMouseEvent* event)
{
  switch(event->button())
  {
    case Qt::LeftButton:
      if (mouse_over_tile != -1)
      {
        drag_obj = mouse_over_tile;

        if (drag_obj != -1)
        {
          QDrag* drag = new QDrag(this);
          QMimeData* mimeData = new QMimeData;
          SuperTuxBadGuyData object;
          QByteArray data(reinterpret_cast<const char*>(&object), sizeof(object));
          mimeData->setData("application/supertux-badguy", data);
          drag->setMimeData(mimeData);

          drag->setPixmap(QPixmap::fromImage(m_brushes[drag_obj].get_sprite().get_pixelbuffer().get_qimage()));
          drag->setHotSpot(QPoint(m_brushes[drag_obj].get_sprite().get_width()/2,
                                  m_brushes[drag_obj].get_sprite().get_height()/2));

          std::cout << "Starting drag" << std::endl;
          /*Qt::DropAction result =*/ drag->exec();
          std::cout << "Starting drag finished" << std::endl;

          drag_obj = -1;
        }
      }
      break;

    case Qt::MidButton:
      scrolling = true;
      click_pos = Point(event->pos());
      old_offset = offset;
      //GRUMBEL: ui->scrollArea->horizontalScrollBar()->setValue(100);
      releaseMouse();
      break;

    default:
      break;
  }
}

void
ObjectSelectorWidget::mouseReleaseEvent(QMouseEvent* event)
{
  switch(event->button())
  {
    case Qt::LeftButton:
      if (drag_obj != -1)
      {
        //releaseMouse();

#ifdef GRUMBEL
        if (!hasFocus())
        {
          Point screen(event->x() + get_screen_rect().left,
                       event->x() + get_screen_rect().top);

          Point target(screen.x - EditorMapComponent::current()->get_screen_rect().left,
                       screen.y - EditorMapComponent::current()->get_screen_rect().top);

          // FIXME: Move this to the scripting layer
          //ObjectAddCommand command(ObjectLayer::current());

          //ObjMapObject obj = m_brushes[drag_obj].to_sprite_object
          //(EditorMapComponent::current()->screen2world(target)).to_object();

          //command.add_object(obj);
          //Workspace::current().get_map().execute(command.to_command());

          //std::cout << "C++: Calling on_drop" << std::endl;
          on_drop(m_brushes[drag_obj], target);
          //std::cout << "C++: Calling on_drop: done" << std::endl;
        }
#endif
        drag_obj = -1;
      }
      break;

    case Qt::MidButton:
      scrolling = false;
      releaseMouse();
      break;

    default:
      break;
  }
}

void
ObjectSelectorWidget::mouseMoveEvent(QMouseEvent* event)
{
  if (scrolling)
  {
    offset = old_offset + (click_pos.y - event->y());
  }

  mouse_pos = Point(event->pos());

  float cell_w = static_cast<float>(width()) / get_columns();
  int x = event->x() / static_cast<int>(cell_w);
  int y = (event->y() + offset) / static_cast<int>(m_cell_height);

  mouse_over_tile = y * get_columns() + x;

  if (mouse_over_tile < 0 || mouse_over_tile >= (int)m_brushes.size())
  {
    mouse_over_tile = -1;
  }

  repaint();
}

void
ObjectSelectorWidget::wheelEvent(QWheelEvent* event)
{
  int numDegrees = event->delta() / 8;
  int numSteps = numDegrees / 15;

  offset += static_cast<int>(m_cell_height * scale * numSteps);
}

void
ObjectSelectorWidget::paintEvent(QPaintEvent* event)
{
  if (offset < 0)
    offset = 0;

  QPainter painter(this);
  GraphicContext gc(painter);

  for(int i = 0; i < static_cast<int>(m_brushes.size()); ++i)
  {
    int x = i % get_columns();
    int y = i / get_columns();

    float cell_w = static_cast<float>(width()) / get_columns();
    Rectf rect(x * cell_w, y * m_cell_height,
               (x+1) * cell_w, (y+1) * m_cell_height);

    if ((x + y - 1) % 2 == 0)
    {
      gc.fill_rect(rect, Color(224, 224, 224));
    }
    else
    {
      gc.fill_rect(rect, Color(192, 192, 192));
    }

    Sprite sprite = m_brushes[i].get_sprite();
    sprite.set_alignment(Flexlay_origin_center, 0, 0);
    sprite.set_scale(std::min(1.0f, (float)m_cell_width / (float)sprite.get_width()),
                     std::min(1.0f, (float)m_cell_height / (float)sprite.get_height()));
    sprite.draw(rect.left + rect.get_width() / 2,
                rect.top + rect.get_height() / 2,
                gc);

    // highlight the current selection
    if (mouse_over_tile == i && m_has_focus)
    {
      gc.fill_rect(rect, Color(0, 0, 255, 20));
    }
  }
}

void
ObjectSelectorWidget::enterEvent(QEvent* event)
{
  m_has_focus = true;
}

void
ObjectSelectorWidget::leaveEvent(QEvent* event)
{
  m_has_focus = false;
  repaint();
}

void
ObjectSelectorWidget::add_brush(const ObjectBrush& brush)
{
  m_brushes.push_back(brush);
}

boost::signals2::signal<void (ObjectBrush, Point)>&
ObjectSelectorWidget::sig_drop()
{
  return on_drop;
}

/* EOF */
