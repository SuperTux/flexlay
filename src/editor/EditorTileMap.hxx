//  $Id: EditorTileMap.hxx,v 1.1 2002/03/19 17:56:59 grumbel Exp $
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

#include "../Field.hxx"
#include "EditorTile.hxx"

/** Holds the tilemap data for the editor and provides functions to
    manipulate them */
class EditorTileMap
{
private:
  Field<EditorTile*>* field;
  typedef Field<EditorTile*>::iterator FieldIter;
public:
  EditorTileMap (int arg_width, int arg_height);
  EditorTileMap (const std::string& filename);
  
  void draw ();
  EditorTile* get_tile (int, int);

  void save (const std::string& filename); 
};

#endif

/* EOF */
