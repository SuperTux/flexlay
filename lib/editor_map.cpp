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
// (assert removed — runtime checks used instead)
#include <iostream>

#include "meta_data.hpp"
#include <ClanLib/Signals/signal_v0.h>

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

  CL_Color background_color;
  CL_Color foreground_color;

  /** Metadata attached to this map (ie. mapname, description, scripts, etc.) */
  MetaData metadata;

  typedef std::vector<Command> Commands;

  Commands undo_stack;
  Commands redo_stack;

  CL_Signal_v0 on_change;

  bool    has_bounding_rect;
  CL_Rect bounding_rect;
};

EditorMap::EditorMap(bool create) :
  impl()
{
  if (create)
  {
    impl.reset(new EditorMapImpl());
    impl->background_color = CL_Color(100, 80, 100);
    impl->foreground_color = CL_Color(255, 80, 255);
    impl->modified = false;
    impl->serial = 0;
    impl->has_bounding_rect = false;
    impl->bounding_rect = CL_Rect(0,0,0,0);
  }
}

void
EditorMap::add_layer(const Layer& layer, int pos)
{
  if (!impl.get())
    return;
  std::cout << impl << " EditorMap::add_layer" << std::endl;

  // pos == -1 appends; otherwise must be a valid insert index.
  if (pos != -1 && (pos < 0 || pos > int(impl->layers.size())))
  {
    std::cout << "EditorMap::add_layer: invalid pos " << pos
              << " (size=" << impl->layers.size() << "), appending" << std::endl;
    pos = -1;
  }

  if (pos == -1) // insert at last pos
    impl->layers.push_back(layer);
  else
    impl->layers.insert(impl->layers.begin() + pos, layer);

  impl->serial += 1;
}

void
EditorMap::draw_gui(CL_GraphicContext* gc)
{
  if (!impl.get())
    return;
  CL_Rect rect = get_bounding_rect();

  if (rect != CL_Rect(0,0,0,0))
  {
    gc->fill_rect(rect, impl->background_color);
    gc->draw_rect(rect, impl->foreground_color);
  }
  else
  {
    gc->clear(impl->background_color);
  }
}

void
EditorMap::draw(const GraphicContextState& state, CL_GraphicContext* gc)
{
  if (!impl.get())
    return;
  for(EditorMapImpl::Layers::iterator i = impl->layers.begin(); i != impl->layers.end(); ++i)
    (*i).draw(state, gc);

  gc->flush();
}

bool
EditorMap::is_modified() const
{
  if (!impl.get())
    return false;
  return impl->modified;
}

void
EditorMap::set_unmodified()
{
  if (!impl.get())
    return;
  impl->modified = false;
}

void
EditorMap::modify()
{
  if (!impl.get())
    return;
  impl->modified = true;
  impl->serial += 1;
}

int
EditorMap::get_serial() const
{
  if (!impl.get())
    return 0;
  return impl->serial;
}

int
EditorMap::get_layer_count() const
{
  if (!impl.get())
    return 0;
  return static_cast<int>(impl->layers.size());
}

Layer
EditorMap::get_layer(int i)
{
  if (!impl.get())
    return Layer();
  if (i >= 0 && i < static_cast<int>(impl->layers.size()))
    return impl->layers[i];
  else
    return Layer();
}

void
EditorMap::set_metadata(const MetaData& obj)
{
  if (!impl.get())
    return;
  impl->metadata = obj;
}

MetaData
EditorMap::get_metadata() const
{
  if (!impl.get())
    return MetaData();
  return impl->metadata;
}

bool
EditorMap::has_bounding_rect() const
{
  if (!impl.get())
    return false;
  return impl->has_bounding_rect;
}

void
EditorMap::set_bounding_rect(const CL_Rect& rect)
{
  if (!impl.get())
    return;
  if (rect != CL_Rect(0,0,0,0))
  {
    impl->has_bounding_rect = true;
    impl->bounding_rect     = rect;
  }
  else
  {
    impl->has_bounding_rect = false;
    impl->bounding_rect     = rect;
  }
}

CL_Rect
EditorMap::get_bounding_rect()
{
  if (!impl.get())
    return CL_Rect(0,0,0,0);
  if (impl->has_bounding_rect)
  {
    return impl->bounding_rect;
  }
  else
  {
    bool init = false;
    CL_Rect rect(0,0,0,0);

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
          CL_Rect other = i->get_bounding_rect();
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
EditorMap::set_background_color(const CL_Color& color)
{
  if (!impl.get())
    return;
  impl-> background_color = color;
}

void
EditorMap::execute(Command command)
{
  if (!impl.get())
    return;
  impl->redo_stack.clear();
  command.execute();
  impl->undo_stack.push_back(command);
  // Cap undo history to avoid unbounded memory growth.
  const std::size_t kMaxUndo = 100;
  while (impl->undo_stack.size() > kMaxUndo)
    impl->undo_stack.erase(impl->undo_stack.begin());
  impl->modified = true;
  impl->serial += 1;
  impl->on_change();
}

void
EditorMap::undo()
{
  if (!impl.get())
    return;
  if (!impl->undo_stack.empty())
  {
    Command command = impl->undo_stack.back();
    impl->undo_stack.pop_back();
    command.undo();
    impl->redo_stack.push_back(command);
    impl->modified = true;
    impl->serial += 1;
    impl->on_change();
  }
}

void
EditorMap::redo()
{
  if (!impl.get())
    return;
  if (!impl->redo_stack.empty())
  {
    Command command = impl->redo_stack.back();
    impl->redo_stack.pop_back();
    command.redo();
    impl->undo_stack.push_back(command);
    impl->modified = true;
    impl->serial += 1;
    impl->on_change();
  }
}

int
EditorMap::undo_stack_size()
{
  if (!impl.get())
    return 0;
  return impl->undo_stack.size();
}

int
EditorMap::redo_stack_size()
{
  if (!impl.get())
    return 0;
  return impl->redo_stack.size();
}

CL_Signal_v0&
EditorMap::sig_change()
{
  if (!impl.get())
  {
    static CL_Signal_v0 dummy;
    return dummy;
  }
  return impl->on_change;
}

/* EOF */
