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

#ifndef HEADER_EDITOR_MAPSIZE_LAYER_HXX
#define HEADER_EDITOR_MAPSIZE_LAYER_HXX

#include <ClanLib/Core/Math/rect.h>
#include "editor_map_layer.hxx"

/** Simple layer that displays the map size, only needed in case the
    mapsize isn't clear out of the other layers itself */
class EditorMapsizeLayer : public EditorMapLayer
{
private:
  CL_Rect rect;
public:
  EditorMapsizeLayer(const CL_Rect& r);

  void draw(EditorMapComponent* parent);

  bool has_bounding_rect() const { return true; }
  CL_Rect get_bounding_rect() { return rect; }
  void set_bounding_rect(const CL_Rect& r) { rect = r; }
  
private:
  EditorMapsizeLayer (const EditorMapsizeLayer&);
  EditorMapsizeLayer& operator= (const EditorMapsizeLayer&);
};

#endif

/* EOF */
