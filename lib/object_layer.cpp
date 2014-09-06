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

#include "object_layer.hpp"

#include "layer_impl.hpp"
#include "graphic_context.hpp"
#include "graphic_context_state.hpp"

ObjectLayer ObjectLayer::current_;

class ObjectLayerImpl : public LayerImpl
{
public:
  ObjectLayer::Objects objects;
  ObjectLayer::ControlPoints control_points;
  std::vector<boost::signals2::connection> slots;

  ObjectLayerImpl() {}
  virtual ~ObjectLayerImpl() {}

  void draw(GraphicContext& gc);
  bool has_bounding_rect() const { return false; }
};

ObjectLayer::ObjectLayer()
  : impl(new ObjectLayerImpl)
{
}

ObjectLayer::~ObjectLayer()
{
}

void
ObjectLayerImpl::draw(GraphicContext& gc)
{
  for(ObjectLayer::Objects::iterator i = objects.begin(); i != objects.end(); ++i)
  {
    // FIXME: Add clipping here
    if (gc.get_clip_rect().is_overlapped((*i).get_bound_rect()))
    {
      (*i).draw(gc);
    }
  }

  for(ObjectLayer::ControlPoints::iterator i = control_points.begin(); i != control_points.end(); ++i)
  {
    (*i).draw(gc);
  }
}

ObjMapControlPoint
ObjectLayer::find_control_point(const Pointf& click_pos)
{
  for(ControlPoints::reverse_iterator i = impl->control_points.rbegin();
      i != impl->control_points.rend();
      ++i)
  {
    Rect rect = (*i).get_bound_rect();

    if (rect.is_inside(Point(click_pos)))
      return *i;
  }
  return ObjMapControlPoint();
}

ObjMapObject
ObjectLayer::find_object(const Pointf& click_pos)
{
  for(Objects::reverse_iterator i = impl->objects.rbegin(); i != impl->objects.rend(); ++i)
  {
    Rectf rect = (*i).get_bound_rect();

    if (rect.is_inside(Point(click_pos)))
      return *i;
  }
  return ObjMapObject();
}

void
ObjectLayer::delete_object(const ObjMapObject& obj)
{
  for(Objects::iterator i = impl->objects.begin(); i != impl->objects.end(); ++i)
  {
    if (obj == (*i))
    {
      impl->objects.erase(i);
      break;
    }
  }
}

ObjectLayer::Objects
ObjectLayer::get_selection(const Rectf& rect)
{
  Objects selection;

  for(Objects::iterator i = impl->objects.begin(); i != impl->objects.end(); ++i)
  {
    // FIXME:
    if (rect.is_inside((*i).get_pos()))
    {
      selection.push_back(*i);
    }
  }

  return selection;
}

ObjectLayer::Objects
ObjectLayer::get_objects()
{
  return impl->objects;
}

void
ObjectLayer::add_object(const ObjMapObject& obj)
{
  impl->objects.push_back(obj);
}

void
ObjectLayer::add_control_point(const ObjMapControlPoint& obj)
{
  impl->control_points.push_back(obj);
}

void
ObjectLayer::delete_control_points()
{
  impl->control_points.clear();
}

Layer
ObjectLayer::to_layer()
{
  return Layer(impl);
}

int
ObjectLayer::get_object_index(const ObjMapObject& obj)
{
  Objects::size_type i;
  for(i = 0; i < impl->objects.size(); ++i)
  {
    if (impl->objects[i] == obj)
    {
      return i;
    }
  }
  return -1;
}

void
ObjectLayer::move_to(const ObjMapObject& obj, int height)
{
  // FIXME: Implement me
}

void
ObjectLayer::raise(const ObjMapObject& obj)
{
  int i = get_object_index(obj);
  if (i != -1 && impl->objects.size() > 1 && i < int(impl->objects.size())-1)
  {
    std::swap(impl->objects[i], impl->objects[i+1]);
  }
}

void
ObjectLayer::lower(const ObjMapObject& obj)
{
  int i = get_object_index(obj);
  if (i != -1 && i > 0)
  {
    std::swap(impl->objects[i], impl->objects[i-1]);
  }
}

/* EOF */
