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

#include <assert.h>
#include <iostream>
#include <ClanLib/Core/core_iostream.h>
#include <ClanLib/Core/Math/rect.h>
#include "editor_tilemap.hxx"
#include "paint_command.hxx"

PaintCommand::PaintCommand(Field<int>* f, const TileBrush& b)
  : field(f)
{  
  undo_field = *field;

  redo_brush = 0;
  undo_brush = 0;
}

PaintCommand::~PaintCommand()
{
  delete redo_brush;
  delete undo_brush;
}

void
PaintCommand::add_point(const CL_Point& pos)
{
  points.push_back(pos);
}

void
PaintCommand::execute()
{
  assert(!points.empty());
  
  // Calc bounding rect
  CL_Rect rect(points.front().x, 
               points.front().y, 
               points.front().x + 1,
               points.front().y + 1);

  for(Points::iterator i = points.begin(); i != points.end(); ++i)
    {
      rect.left   = std::min(rect.left,   (*i).x);
      rect.top    = std::min(rect.top,    (*i).y);
      rect.right  = std::max(rect.right,  (*i).x + 1);
      rect.bottom = std::max(rect.bottom, (*i).y + 1);
    }
  
  pos.x = rect.left;
  pos.y = rect.top;

  redo_brush = new TileBrush(*field,     rect.get_width(), rect.get_height(), -pos.x, -pos.y);
  undo_brush = new TileBrush(undo_field, rect.get_width(), rect.get_height(), -pos.x, -pos.y);
  
  redo_brush->set_opaque();
  undo_brush->set_opaque();

  undo_field.clear();
}

void
PaintCommand::redo()
{
  EditorTileMap::draw_tile(field, *redo_brush, pos);
}

void
PaintCommand::undo()
{
  EditorTileMap::draw_tile(field, *undo_brush, pos);
}

/* EOF */
