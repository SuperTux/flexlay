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

#include <assert.h>
#include <iostream>
#include <sstream>
#include <vector>
#include <ClanLib/Core/core_iostream.h>
#include <ClanLib/Core/Math/rect.h>
#include "field.hxx"
#include "tilemap_layer.hxx"
#include "paint_command.hxx"

class PaintCommandImpl
{
public:
  typedef std::vector<CL_Point> Points;
  Points points;
  
  TilemapLayer tilemap;
  TileBrush    brush;

  /** Copy of the field used to generate undo informations */
  Field<int>   undo_field;

  CL_Point     pos;
  TileBrush*   redo_brush;
  TileBrush*   undo_brush;
};

PaintCommand::PaintCommand(TilemapLayer t, const TileBrush& b)
  : impl(new PaintCommandImpl())
{  
  impl->tilemap = t;
  impl->brush   = b;

  impl->undo_field = *(impl->tilemap.get_field());

  impl->redo_brush = 0;
  impl->undo_brush = 0;
}

PaintCommand::~PaintCommand()
{
  delete impl->redo_brush;
  delete impl->undo_brush;
}

void
PaintCommand::add_point(const CL_Point& pos)
{
  impl->points.push_back(pos);
  impl->tilemap.draw_tile(impl->brush, pos);
}

void
PaintCommand::execute()
{
  assert(!impl->points.empty());
  
  // Calc bounding rect
  CL_Rect rect(impl->points.front().x, 
               impl->points.front().y, 
               impl->points.front().x + impl->brush.get_width(),
               impl->points.front().y + impl->brush.get_height());

  for(PaintCommandImpl::Points::iterator i = impl->points.begin(); i != impl->points.end(); ++i)
    {
      rect.left   = std::min(rect.left,   (*i).x);
      rect.top    = std::min(rect.top,    (*i).y);
      rect.right  = std::max(rect.right,  (*i).x + impl->brush.get_width());
      rect.bottom = std::max(rect.bottom, (*i).y + impl->brush.get_height());
    }
  
  impl->pos.x = rect.left;
  impl->pos.y = rect.top;

  impl->redo_brush = new TileBrush(*(impl->tilemap.get_field()), rect.get_width(), rect.get_height(),
                                   -impl->pos.x, -impl->pos.y);
  impl->undo_brush = new TileBrush(impl->undo_field, rect.get_width(), rect.get_height(), 
                                   -impl->pos.x, -impl->pos.y);
  
  impl->redo_brush->set_opaque();
  impl->undo_brush->set_opaque();

  impl->undo_field.clear();
}

void
PaintCommand::redo()
{
  TilemapLayer::draw_tile(impl->tilemap.get_field(), *impl->redo_brush, impl->pos);
}

void
PaintCommand::undo()
{
  TilemapLayer::draw_tile(impl->tilemap.get_field(), *impl->undo_brush, impl->pos);
}

std::string
PaintCommand::serialize()
{
  std::stringstream s;

  s << "_ = PaintCommand(" << &impl->tilemap << ", " << &impl->brush << ")" << std::endl;
  for(PaintCommandImpl::Points::iterator i = impl->points.begin(); i != impl->points.end(); ++i)
    {
      s << "_.add_paint(" << i->x << ", " << i->y << ")"  << std::endl;
    }
  s << "_ = None" << std::endl;

  return s.str();
}

/* EOF */
