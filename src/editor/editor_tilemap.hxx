//  $Id: editor_tilemap.hxx,v 1.3 2003/09/10 13:53:11 grumbel Exp $
// 
//  Pingus - A free Lemmings clone
//  Copyright (C) 2000 Ingo Ruhnke <grumbel@gmx.de>
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

#ifndef EDITORTILEMAP_HXX
#define EDITORTILEMAP_HXX

#include <ClanLib/gui.h>
#include "../field.hxx"
#include "editor_tile.hxx"

/** Holds the tilemap data for the editor and provides functions to
    manipulate them */
class EditorTileMap : public CL_Component
{
private:
  CL_SlotContainer slots;
  Field<EditorTile*>* field;
  typedef Field<EditorTile*>::iterator FieldIter;
  
  CL_Pointf trans_offset;
  CL_Pointf old_trans_offset;

  CL_Point click_pos;

  enum { NONE, SCROLLING, PAINTING } tool;
public:
  int brush_tile;

  EditorTileMap (CL_Component* parent);
  
  void draw ();

  void mouse_up(const CL_InputEvent& event);
  void mouse_down(const CL_InputEvent& event);
  void mouse_move(const CL_InputEvent& event);

  EditorTile* get_tile (int, int);

  void load (const std::string& filename);
  void save (const std::string& filename);

  CL_Point screen2tile(const CL_Point& pos);
};

#endif

/* EOF */
