//  $Id$
//
//  Pingus - A free Lemmings clone
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
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/keys.h>
#include "../windstille_level.hxx"
#include "../globals.hxx"
#include "editor_names.hxx"
#include "editor_map.hxx"
#include "editor_map_component.hxx"

EditorMap::EditorMap()
{
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
}

void
EditorMap::draw (EditorMapComponent* parent)
{
  CL_Display::clear(CL_Color(100, 0, 100));
  for(Layers::iterator i = layers.begin(); i != layers.end(); ++i)
    (*i)->draw(parent);  
  CL_Display::flush();
}

EditorMapLayer*
EditorMap::get_layer_by_name(int i)
{
  switch(i)
    {
    case TILEMAP_NAME:
      return tilemap;
    case OBJECTMAP_NAME: 
      return objmap;
    default:
      return 0;
    }
}

EditorMapLayer*
EditorMap::get_layer(int i)
{
  if (i >= 0 && i < static_cast<int>(layers.size()))
    return layers[i];
  else
    return 0;
}

/* EOF */
