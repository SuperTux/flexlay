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
#ifdef SWIGGUILE
  SCMObj metadata;
#endif
};

EditorMap::EditorMap()
  : impl(new EditorMapImpl())
{
  impl->background_color = CL_Color(100, 80, 100);
  impl->foreground_color = CL_Color(255, 80, 255);
  impl->modified = false;
  impl->serial = 0;
}

void
EditorMap::add_layer(const Layer& layer)
{
  impl->layers.push_back(layer);
  impl->serial += 1;
}

void
EditorMap::draw (EditorMapComponent* parent)
{
  CL_Rect rect = get_bounding_rect();

  CL_Display::fill_rect(rect, impl->background_color);
  CL_Display::draw_rect(rect, impl->foreground_color);
  
  for(EditorMapImpl::Layers::iterator i = impl->layers.begin(); i != impl->layers.end(); ++i)
    (*i).draw(parent);
  
  CL_Display::flush();
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

Layer
EditorMap::get_layer(int i)
{
  if (i >= 0 && i < static_cast<int>(impl->layers.size()))
    return impl->layers[i];
  else
    return Layer();
}

#ifdef SWIGGUILE
void
EditorMap::set_metadata(const SCMObj& obj)
{
  impl->metadata = obj; 
}

SCMObj
EditorMap::get_metadata() const
{
  return impl->metadata; 
}
#endif

CL_Rect
EditorMap::get_bounding_rect()
{
  assert(impl->layers.size() >= 1);
  
  bool init = false;
  CL_Rect rect;

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

void
EditorMap::set_background_color(const CL_Color& color)
{
 impl-> background_color = color;
}

/* EOF */
