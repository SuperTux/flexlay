//  $Id$
//
//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

#include <iostream>
#include <assert.h>
#include <ClanLib/Core/core_iostream.h>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/keys.h>
#include "editor_names.hxx"
#include "editor_map.hxx"
#include "editor_map_component.hxx"

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

EditorMap::EditorMap()
  : impl(new EditorMapImpl())
{
  impl->background_color = CL_Color(100, 80, 100);
  impl->foreground_color = CL_Color(255, 80, 255);
  impl->modified = false;
  impl->serial = 0;
  impl->has_bounding_rect = false;
  impl->bounding_rect = CL_Rect(0,0,0,0);
}

void
EditorMap::add_layer(const Layer& layer, int pos)
{
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
EditorMap::draw (EditorMapComponent* parent, CL_GraphicContext* gc)
{
  for(EditorMapImpl::Layers::iterator i = impl->layers.begin(); i != impl->layers.end(); ++i)
    (*i).draw(parent, gc);
  
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
EditorMap::set_bounding_rect(const CL_Rect& rect)
{
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
  impl-> background_color = color;
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

CL_Signal_v0&
EditorMap::sig_change()
{
  return impl->on_change;
}

/* EOF */
