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

EditorMap::EditorMap(const std::string& filename_)
  : filename(filename_),
    background_color(100, 80, 100),
    foreground_color(255, 80, 255)
{
  modified = false;
  serial = 0;
}

EditorMap::~EditorMap()
{
  scripts.clear();
  for(Layers::iterator i = layers.begin(); i != layers.end(); ++i)
    {
      delete (*i);
    }
}

void
EditorMap::add_layer(EditorMapLayer* layer)
{
  layers.push_back(layer);
  ++serial;
}

void
EditorMap::draw (EditorMapComponent* parent)
{
  CL_Rect rect = get_bounding_rect();

  CL_Display::fill_rect(rect, background_color);
  CL_Display::draw_rect(rect, foreground_color);
  for(Layers::iterator i = layers.begin(); i != layers.end(); ++i)
    (*i)->draw(parent);  
  CL_Display::flush();
}

EditorMapLayer*
EditorMap::get_layer_by_name(int i)
{
  return layers[i];
}

EditorMapLayer*
EditorMap::get_layer(int i)
{
  if (i >= 0 && i < static_cast<int>(layers.size()))
    return layers[i];
  else
    return 0;
}

#ifdef SWIGGUILE
void
EditorMap::set_metadata(const SCMObj& obj)
{
  metadata = obj; 
}

SCMObj
EditorMap::get_metadata() const
{
  return metadata; 
}
#endif

CL_Rect
EditorMap::get_bounding_rect()
{
  assert(layers.size() >= 1);
  
  bool init = false;
  CL_Rect rect;
  
  for(Layers::iterator i = layers.begin(); i != layers.end(); ++i)
    {
      if ((*i)->has_bounding_rect())
        {
          if (!init)
            {
              rect = (*i)->get_bounding_rect();
              init = true;
            }
          else
            {
              CL_Rect other = (*i)->get_bounding_rect();
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
  background_color = color;
}

/* EOF */
