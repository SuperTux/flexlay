//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "editor_map.hpp"

#include <ClanLib/Display/graphic_context.h>
#include <assert.h>
#include <iostream>

#include "math/rect.hpp"
#include "meta_data.hpp"

class EditorMapImpl
{
public:
  /** Flag if the map got modified, used for 'Some maps are unsaved'
      style massages */
  bool modified;

  /** Gets incremented with each map change so that other component
      can update if required */
  int serial;

  typedef std::vector<Layer> Layers;
  Layers layers;

  Color background_color;
  Color foreground_color;

  /** Metadata attached to this map (ie. mapname, description, scripts, etc.) */
  MetaData metadata;

  typedef std::vector<Command> Commands;

  Commands undo_stack;
  Commands redo_stack;

  boost::signals2::signal<void ()> on_change;

  bool    has_bounding_rect;
  Rect bounding_rect;
};

EditorMap::EditorMap(bool create) :
  impl()
{
  if (create)
  {
    impl.reset(new EditorMapImpl());
    impl->background_color = Color(100, 80, 100);
    impl->foreground_color = Color(255, 80, 255);
    impl->modified = false;
    impl->serial = 0;
    impl->has_bounding_rect = false;
    impl->bounding_rect = Rect(0,0,0,0);
  }
}

void
EditorMap::add_layer(const Layer& layer, int pos)
{
  std::cout << impl << " EditorMap::add_layer" << std::endl;

  assert(pos == -1 || (pos >= 0 && pos < int(impl->layers.size())));

  if (pos == -1) // insert at last pos
    impl->layers.push_back(layer);
  else
    impl->layers.insert(impl->layers.begin() + pos, layer);

  impl->serial += 1;
}

void
EditorMap::draw_gui(CL_GraphicContext* gc)
{
  Rect rect = get_bounding_rect();

  if (rect != Rect(0,0,0,0))
  {
    gc->fill_rect(rect.to_cl(), impl->background_color.to_cl());
    gc->draw_rect(rect.to_cl(), impl->foreground_color.to_cl());
  }
  else
  {
    gc->clear(impl->background_color.to_cl());
  }
}

void
EditorMap::draw(const GraphicContextState& state, CL_GraphicContext* gc)
{
  for(EditorMapImpl::Layers::iterator i = impl->layers.begin(); i != impl->layers.end(); ++i)
    (*i).draw(state, gc);

  gc->flush();
}

bool
EditorMap::is_modified() const
{
  return impl->modified;
}

void
EditorMap::set_unmodified()
{
  impl->modified = false;
}

void
EditorMap::modify()
{
  impl->modified = true;
  impl->serial += 1;
}

int
EditorMap::get_serial() const
{
  return impl->serial;
}

int
EditorMap::get_layer_count() const
{
  return static_cast<int>(impl->layers.size());
}

Layer
EditorMap::get_layer(int i)
{
  if (i >= 0 && i < static_cast<int>(impl->layers.size()))
    return impl->layers[i];
  else
    return Layer();
}

void
EditorMap::set_metadata(const MetaData& obj)
{
  impl->metadata = obj;
}

MetaData
EditorMap::get_metadata() const
{
  return impl->metadata;
}

bool
EditorMap::has_bounding_rect() const
{
  return impl->has_bounding_rect;
}

void
EditorMap::set_bounding_rect(const Rect& rect)
{
  if (rect != Rect(0,0,0,0))
  {
    impl->has_bounding_rect = true;
    impl->bounding_rect = rect;
  }
  else
  {
    impl->has_bounding_rect = false;
    impl->bounding_rect = rect;
  }
}

Rect
EditorMap::get_bounding_rect()
{
  if (impl->has_bounding_rect)
  {
    return impl->bounding_rect;
  }
  else
  {
    bool init = false;
    Rect rect(0,0,0,0);

    for(EditorMapImpl::Layers::iterator i = impl->layers.begin(); i != impl->layers.end(); ++i)
    {
      if (i->has_bounding_rect())
      {
        if (!init)
        {
          rect = i->get_bounding_rect();
          init = true;
        }
        else
        {
          Rect other = i->get_bounding_rect();
          rect.top    = std::min(rect.top,    other.top);
          rect.bottom = std::max(rect.bottom, other.bottom);
          rect.left   = std::min(rect.left,   other.left);
          rect.right  = std::max(rect.right,  other.right);
        }
      }
    }
    return rect;
  }
}

void
EditorMap::set_background_color(const Color& color)
{
  impl->background_color = color;
}

void
EditorMap::execute(Command command)
{
  impl->redo_stack.clear();
  command.execute();
  impl->undo_stack.push_back(command);
  impl->on_change();
}

void
EditorMap::undo()
{
  if (!impl->undo_stack.empty())
  {
    Command command = impl->undo_stack.back();
    impl->undo_stack.pop_back();
    command.undo();
    impl->redo_stack.push_back(command);
    impl->on_change();
  }
}

void
EditorMap::redo()
{
  if (!impl->redo_stack.empty())
  {
    Command command = impl->redo_stack.back();
    impl->redo_stack.pop_back();
    command.redo();
    impl->undo_stack.push_back(command);
    impl->on_change();
  }
}

int
EditorMap::undo_stack_size()
{
  return impl->undo_stack.size();
}

int
EditorMap::redo_stack_size()
{
  return impl->redo_stack.size();
}

boost::signals2::signal<void ()>&
EditorMap::sig_change()
{
  return impl->on_change;
}

/* EOF */
