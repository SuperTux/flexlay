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

#ifndef HEADER_EDITOR_MAP_HXX
#define HEADER_EDITOR_MAP_HXX

#include <vector>
#include <ClanLib/Display/sprite.h>
#include <ClanLib/GUI/component.h>
#include <ClanLib/Core/Math/point.h>
#include "field.hxx"
#include "object_layer.hxx"
#include "tilemap_layer.hxx"
#include "layer.hxx"

class EditorMapComponent;
class EditorMapImpl;
class TileMapTool;

/** Object which represents a level, quirled together with the GUI
    stuff */
class EditorMap
{
public:
  EditorMap();

  void draw(EditorMapComponent* parent);

  void add_layer(const Layer& layer);

  bool is_modified() const;
  void set_unmodified();
  void modify();

  int get_serial() const;

  Layer get_layer(int i);

  void   set_metadata(const MetaData& obj);
  MetaData get_metadata() const;

  bool has_bounding_rect() const { return true; }
  CL_Rect get_bounding_rect();

  void set_background_color(const CL_Color& color);

private:
  CL_SharedPtr<EditorMapImpl> impl;
};

#endif

/* EOF */
