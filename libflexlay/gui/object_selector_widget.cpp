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

#include <QPainter>
#include <QResizeEvent>

#include "graphic_context.hpp"

ObjectSelectorWidget::ObjectSelectorWidget(int obj_w, int obj_h, QWidget* parent) :
  QWidget(parent),
  width(1),
  height(1),
  obj_width(obj_w), 
  obj_height(obj_h),
  offset(0)
{
  index = 0;

  mouse_over_tile = -1;
  scrolling = false;
  scale = 1.0f;
  drag_obj = -1;

  setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
  setMouseTracking(true);
}

ObjectSelectorWidget::~ObjectSelectorWidget()
{
}

QSize
ObjectSelectorWidget::minimumSizeHint() const
{
  return QSize(32, 32);
}
  
QSize
ObjectSelectorWidget::sizeHint() const
{
  return QSize(1280, 800);
}

void
ObjectSelectorWidget::resizeEvent(QResizeEvent* event)
{
  width = event->size().width() / obj_width;
  height = event->size().height() / obj_height;
}

void
ObjectSelectorWidget::mouseReleaseEvent(QMouseEvent* event)
{
  switch(event->button())
  {
    case Qt::LeftButton:
      if (drag_obj != -1)
      {
        releaseMouse();

#ifdef GRUMBEL
        if (!hasFocus())
        {
          Point screen(event->x() + get_screen_rect().left,
                       event->x() + get_screen_rect().top);

          Point target(screen.x - EditorMapComponent::current()->get_screen_rect().left,
                       screen.y - EditorMapComponent::current()->get_screen_rect().top);

          // FIXME: Move this to the scripting layer
          //ObjectAddCommand command(ObjectLayer::current());

          //ObjMapObject obj = brushes[drag_obj].to_sprite_object
          //(EditorMapComponent::current()->screen2world(target)).to_object();

          //command.add_object(obj);
          //Workspace::current().get_map().execute(command.to_command());

          //std::cout << "C++: Calling on_drop" << std::endl;
          on_drop(brushes[drag_obj], target);
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
ObjectSelectorWidget::mousePressEvent(QMouseEvent* event)
{
  switch(event->button())
  {
    case Qt::LeftButton:
      if (mouse_over_tile != -1)
      {
        drag_obj = mouse_over_tile;
        grabMouse();
      }
      break;

    case Qt::MidButton:
      scrolling = true;
      click_pos = Point(event->pos());
      old_offset = offset;
      releaseMouse();
      break;

    default:
      break;
  }
}

void
ObjectSelectorWidget::wheelEvent(QWheelEvent* event)
{
  int numDegrees = event->delta() / 8;
  int numSteps = numDegrees / 15;

  offset += static_cast<int>(obj_height * scale * numSteps);
}

void
ObjectSelectorWidget::mouseMoveEvent(QMouseEvent* event)
{
  if (scrolling)
  {
    offset = old_offset + (click_pos.y - event->y());
  }

  mouse_pos = Point(event->pos());

  int x = event->x() / static_cast<int>(obj_width);
  int y = (event->y() + offset) / static_cast<int>(obj_height);

  mouse_over_tile = y * width + x;

  if (mouse_over_tile < 0 || mouse_over_tile >= (int)brushes.size())
    mouse_over_tile = -1;

  repaint();
}

void
ObjectSelectorWidget::paintEvent(QPaintEvent* event)
{
  if (offset < 0)
    offset = 0;

  QPainter painter(this);
  GraphicContext gc(painter);

  // Handle scrolling in the Component
  gc.push_modelview();
  gc.translate(0, -offset);
  ////gc.translate(get_screen_x(), get_screen_y());

  for(int i = 0; i < (int)brushes.size(); ++i)
  {
    int x = i % width;
    int y = i / width;

    Rectf rect(Pointf(x * obj_width, y * obj_height),
               Sizef(obj_width, obj_height));

    Sprite sprite = brushes[i].get_sprite();
    sprite.set_alignment(Flexlay_origin_center, 0, 0);
    sprite.set_scale(std::min(1.0f, (float)obj_width / (float)sprite.get_width()),
                     std::min(1.0f, (float)obj_height / (float)sprite.get_height()));

    sprite.draw(x * obj_width + obj_width/2,
                y * obj_height + obj_height/2, gc);

    //std::cout << "Brush: " << rect.left << " " << rect.top << " " << rect.get_width() << "x" << rect.get_width() << std::endl;
    gc.draw_rect(rect, Color(0,0,0,128));

    if (mouse_over_tile == i)//// && hasFocus())
    {
      gc.fill_rect(Rect(rect), Color(0, 0, 255, 20));
    }
  }

  gc.pop_modelview();

  // Draw drag sprite
  if (drag_obj != -1)
  {
#ifdef GRUMBEL
    gc.set_cliprect(Rect(Point(0, 0),
                         Size(gc.get_width(),
                              gc.get_height())));

    Sprite sprite = brushes[drag_obj].get_sprite();
    sprite.set_alpha(0.5f);
    sprite.draw(mouse_pos.x + get_screen_x(), mouse_pos.y + get_screen_y());
#endif
  }
}

void
ObjectSelectorWidget::add_brush(const ObjectBrush& brush)
{
  brushes.push_back(brush);
}

boost::signals2::signal<void (ObjectBrush, Point)>&
ObjectSelectorWidget::sig_drop()
{
  return on_drop;
}

/* EOF */
